# Copyright (C) 2003-2008 Brailcom, o.p.s.
#
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU Lesser General Public License as published by
# the Free Software Foundation; either version 2.1 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU Lesser General Public License for more details.
#
# You should have received a copy of the GNU Lesser General Public License
# along with this program.  If not, see <https://www.gnu.org/licenses/>.

"""Python API to Speech Dispatcher

Basic Python client API to Speech Dispatcher is provided by the 'SSIPClient'
class.  This interface maps directly to available SSIP commands and logic.

A more convenient interface is provided by the 'Speaker' class.

"""

#TODO: Blocking variants for speak, char, key, sound_icon.

import socket, sys, os, subprocess, time, tempfile

try:
    import threading
except:
    import dummy_threading as threading

from . import paths
    
class CallbackType(object):
    """Constants describing the available types of callbacks"""
    INDEX_MARK = 'index_marks'
    """Index mark events are reported when the place they were
    included into the text by the client application is reached
    when speaking them"""
    BEGIN = 'begin'
    """The begin event is reported when Speech Dispatcher starts
    actually speaking the message."""
    END = 'end'
    """The end event is reported after the message has terminated and
    there is no longer any sound from it being produced"""
    CANCEL = 'cancel'
    """The cancel event is reported when a message is canceled either
    on request of the user, because of prioritization of messages or
    due to an error"""
    PAUSE = 'pause'
    """The pause event is reported after speaking of a message
    was paused. It no longer produces any audio."""
    RESUME = 'resume'
    """The resume event is reported right after speaking of a message
    was resumed after previous pause."""

class SSIPError(Exception):
    """Common base class for exceptions during SSIP communication."""
    
class SSIPCommunicationError(SSIPError):
    """Exception raised when trying to operate on a closed connection."""

    _additional_exception = None

    def __init__(self, description=None, original_exception=None, **kwargs):
        self._original_exception = original_exception
        self._description = description
        super(SSIPError, self).__init__(**kwargs)

    def original_exception(self):
        """Return the original exception if any

        If this exception is secondary, being caused by a lower
        level exception, return this original exception, otherwise
        None"""
        return self._original_exception

    def set_additional_exception(self, exception):
        """Set an additional exception
        
        See method additional_exception().
        """
        self._additional_exception = exception

    def additional_exception(self):
        """Return an additional exception
        
        Additional exceptions araise from failed attempts to resolve
        the former problem"""
        return self._additional_exception

    def description(self):
        """Return error description"""
        return self._description

    def __str__(self):
        msgs = []
        if self.description():
            msgs.append(self.description())
        if self.original_exception:
            msgs.append("Original error: " + str(self.original_exception()))
        if self.additional_exception:
            msgs.append("Additional error: " + str(self.additional_exception()))
        return "\n".join(msgs)

class SSIPResponseError(Exception):
    def __init__(self, code, msg, data):
        Exception.__init__(self, "%s: %s" % (code, msg))
        self._code = code
        self._msg = msg
        self._data = data

    def code(self):
        """Return the server response error code as integer number."""
        return self._code
        
    def msg(self):
        """Return server response error message as string."""
        return self._msg


class SSIPCommandError(SSIPResponseError):
    """Exception raised on error response after sending command."""

    def command(self):
        """Return the command string which resulted in this error."""
        return self._data

    
class SSIPDataError(SSIPResponseError):
    """Exception raised on error response after sending data."""

    def data(self):
        """Return the data which resulted in this error."""
        return self._data

    
class SpawnError(Exception):
    """Indicates failure in server autospawn."""

class CommunicationMethod(object):
    """Constants describing the possible methods of connection to server."""
    UNIX_SOCKET = 'unix_socket'
    """Unix socket communication using a filesystem path"""
    INET_SOCKET = 'inet_socket'
    """Inet socket communication using a host and port"""

class _SSIP_Connection(object):
    """Implemantation of low level SSIP communication."""
    
    _NEWLINE = b"\r\n"
    _END_OF_DATA_MARKER = b'.'
    _END_OF_DATA_MARKER_ESCAPED = b'..'
    _END_OF_DATA = _NEWLINE + _END_OF_DATA_MARKER + _NEWLINE
    _END_OF_DATA_ESCAPED = _NEWLINE + _END_OF_DATA_MARKER_ESCAPED + _NEWLINE
    # Constants representing \r\n. and \r\n..
    _RAW_DOTLINE = _NEWLINE + _END_OF_DATA_MARKER
    _ESCAPED_DOTLINE = _NEWLINE + _END_OF_DATA_MARKER_ESCAPED

    _CALLBACK_TYPE_MAP = {700: CallbackType.INDEX_MARK,
                          701: CallbackType.BEGIN,
                          702: CallbackType.END,
                          703: CallbackType.CANCEL,
                          704: CallbackType.PAUSE,
                          705: CallbackType.RESUME,
                          }

    def __init__(self, communication_method, socket_path, host, port):
        """Init connection: open the socket to server,
        initialize buffers, launch a communication handling
        thread.
        """

        if communication_method == CommunicationMethod.UNIX_SOCKET:
            socket_family = socket.AF_UNIX
            socket_connect_args = socket_path
        elif communication_method == CommunicationMethod.INET_SOCKET:
            assert host and port
            socket_family = socket.AF_INET
            socket_connect_args = (socket.gethostbyname(host), port)
        else:
            raise ValueError("Unsupported communication method")

        try:
            self._socket = socket.socket(socket_family, socket.SOCK_STREAM)
            self._socket.connect(socket_connect_args)
        except socket.error as ex:
            raise SSIPCommunicationError("Can't open socket using method "
                                         + communication_method,
                                         original_exception = ex)

        self._buffer = b""
        self._com_buffer = []
        self._callback = None
        self._ssip_reply_semaphore = threading.Semaphore(0)
        self._communication_thread = \
                threading.Thread(target=self._communication, kwargs={},
                                 name="SSIP client communication thread",
                                 daemon=True)
        self._communication_thread.start()
    
    def close(self):
        """Close the server connection, destroy the communication thread."""
        # Read-write shutdown here is necessary, otherwise the socket.recv()
        # function in the other thread won't return at last on some platforms.
        try:
            self._socket.shutdown(socket.SHUT_RDWR)
        except socket.error:
            pass
        self._socket.close()
        # Wait for the other thread to terminate
        self._communication_thread.join()
        
    def _communication(self):
        """Handle incomming socket communication.

        Listens for all incomming communication on the socket, dispatches
        events and puts all other replies into self._com_buffer list in the
        already parsed form as (code, msg, data).  Each time a new item is
        appended to the _com_buffer list, the corresponding semaphore
        'self._ssip_reply_semaphore' is incremented.

        This method is designed to run in a separate thread.  The thread can be
        interrupted by closing the socket on which it is listening for
        reading."""

        while True:
            try:
                code, msg, data = self._recv_message()
            except IOError:
                # If the socket has been closed, exit the thread
                sys.exit()
            if code//100 != 7:
                # This is not an index mark nor an event
                self._com_buffer.append((code, msg, data))
                self._ssip_reply_semaphore.release()
                continue
            # Ignore the event if no callback function has been registered.
            if self._callback is not None:
                type = self._CALLBACK_TYPE_MAP[code]
                if type == CallbackType.INDEX_MARK:
                    kwargs = {'index_mark': data[2]}
                else:
                    kwargs = {}
                # Get message and client ID of the event
                msg_id, client_id = map(int, data[:2])
                self._callback(msg_id, client_id, type, **kwargs)
                
                
    def _readline(self):
        """Read one whole line from the socket.

        Blocks until the line delimiter ('_NEWLINE') is read.
        
        """
        pointer = self._buffer.find(self._NEWLINE)
        while pointer == -1:
            try:
                d = self._socket.recv(1024)
            except:
                raise IOError
            if len(d) == 0:
                raise IOError
            self._buffer += d
            pointer = self._buffer.find(self._NEWLINE)
        line = self._buffer[:pointer]
        self._buffer = self._buffer[pointer+len(self._NEWLINE):]
        return line.decode('utf-8')

    def _recv_message(self):
        """Read server response or a callback
        and return the triplet (code, msg, data)."""
        data = []
        c = None
        while True:
            line = self._readline()
            assert len(line) >= 4, "Malformed data received from server!"
            code, sep, text = line[:3], line[3], line[4:]
            assert code.isalnum() and (c is None or code == c) and \
                   sep in ('-', ' '), "Malformed data received from server!"
            if sep == ' ':
                msg = text
                return int(code), msg, tuple(data)
            data.append(text)

    def _recv_response(self):
        """Read server response from the communication thread
        and return the triplet (code, msg, data)."""
        # TODO: This check is dumb but seems to work.  The main thread
        # hangs without it, when the Speech Dispatcher connection is lost.
        if not self._communication_thread.is_alive():
            raise SSIPCommunicationError
        self._ssip_reply_semaphore.acquire()
        # The list is sorted, read the first item
        response = self._com_buffer[0]
        del self._com_buffer[0]
        return response

    def send_command(self, command, *args):
        """Send SSIP command with given arguments and read server response.

        Arguments can be of any data type -- they are all stringified before
        being sent to the server.

        Returns a triplet (code, msg, data), where 'code' is a numeric SSIP
        response code as an integer, 'msg' is an SSIP rsponse message as string
        and 'data' is a tuple of strings (all lines of response data) when a
        response contains some data.
        
        'SSIPCommandError' is raised in case of non 2xx return code.  See SSIP
        documentation for more information about server responses and codes.

        'IOError' is raised when the socket was closed by the remote side.
        
        """
        if __debug__:
            if command in ('SET', 'CANCEL', 'STOP',):
                assert args[0] in (Scope.SELF, Scope.ALL) \
                       or isinstance(args[0], int)
        cmd = ' '.join((command,) + tuple(map(str, args)))
        try:
            self._socket.send(cmd.encode('utf-8') + self._NEWLINE)
        except socket.error:
            raise SSIPCommunicationError("Speech Dispatcher connection lost.")
        code, msg, data = self._recv_response()
        if code//100 != 2:
            raise SSIPCommandError(code, msg, cmd)
        return code, msg, data
        
    def send_data(self, data):
        """Send multiline data and read server response.

        Returned value is the same as for 'send_command()' method.

        'SSIPDataError' is raised in case of non 2xx return code. See SSIP
        documentation for more information about server responses and codes.
        
        'IOError' is raised when the socket was closed by the remote side.
        
        """
        data = data.encode('utf-8')
        # Escape the end-of-data marker even if present at the beginning
        # The start of the string is also the start of a line.
        if data.startswith(self._END_OF_DATA_MARKER):
            l = len(self._END_OF_DATA_MARKER)
            data = self._END_OF_DATA_MARKER_ESCAPED + data[l:]

        # Escape the end of data marker at the start of each subsequent
        # line.  We can do that by simply replacing \r\n. with \r\n..,
        # since the start of a line is immediately preceded by \r\n,
        # when the line is not the beginning of the string.
        data = data.replace(self._RAW_DOTLINE, self._ESCAPED_DOTLINE)

        try:
            self._socket.send(data + self._END_OF_DATA)
        except socket.error:
            raise SSIPCommunicationError("Speech Dispatcher connection lost.")
        code, msg, response_data = self._recv_response()
        if code//100 != 2:
            raise SSIPDataError(code, msg, data)
        return code, msg, response_data

    def set_callback(self, callback):
        """Register a callback function for handling asynchronous events.

        Arguments:
          callback -- a callable object (function) which will be called to
            handle asynchronous events (arguments described below).  Passing
            `None' results in removing the callback function and ignoring
            events.  Just one callback may be registered.  Attempts to register
            a second callback will result in the former callback being
            replaced.

        The callback function must accept three positional arguments
        ('message_id', 'client_id', 'event_type') and an optional keyword
        argument 'index_mark' (when INDEX_MARK events are turned on).

        Note, that setting the callback function doesn't turn the events on.
        The user is responsible to turn them on by sending the appropriate `SET
        NOTIFICATION' command.

        """
        self._callback = callback

class _CallbackHandler(object):
    """Internal object which handles callbacks."""

    def __init__(self, client_id):
        self._client_id = client_id
        self._callbacks = {}
        self._lock = threading.Lock()

    def __call__(self, msg_id, client_id, type, **kwargs):
        if client_id != self._client_id:
            # TODO: does that ever happen?
            return
        self._lock.acquire()
        try:
            try:
                callback, event_types = self._callbacks[msg_id]
            except KeyError:
                pass
            else:
                if event_types is None or type in event_types:
                    callback(type, **kwargs)
                if type in (CallbackType.END, CallbackType.CANCEL):
                    del self._callbacks[msg_id]
        finally:
            self._lock.release()

    def add_callback(self, msg_id,  callback, event_types):
        self._lock.acquire()
        try:
            self._callbacks[msg_id] = (callback, event_types)
        finally:
            self._lock.release()

class Scope(object):
    """An enumeration of valid SSIP command scopes.

    The constants of this class should be used to specify the 'scope' argument
    for the 'Client' methods.

    """    
    SELF = 'self'
    """The command (mostly a setting) applies to current connection only."""
    ALL = 'all'
    """The command applies to all current Speech Dispatcher connections."""

    
class Priority(object):
    """An enumeration of valid SSIP message priorities.

    The constants of this class should be used to specify the 'priority'
    argument for the 'Client' methods.  For more information about message
    priorities and their interaction, see the SSIP documentation.
    
    """
    IMPORTANT = 'important'
    TEXT = 'text'
    MESSAGE = 'message'
    NOTIFICATION = 'notification'
    PROGRESS = 'progress'

    
class PunctuationMode(object):
    """Constants for selecting a punctuation mode.

    The mode determines which characters should be read.

    """
    ALL = 'all'
    """Read all punctuation characters."""
    NONE = 'none'
    """Don't read any punctuation character at all."""
    SOME = 'some'
    """Only some of the user-defined punctuation characters are read."""
    MOST = 'most'
    """Only most of the user-defined punctuation characters are read.

    The set of characters is specified in Speech Dispatcher configuration.

    """

class DataMode(object):
    """Constants specifying the type of data contained within messages
    to be spoken.

    """
    TEXT = 'text'
    """Data is plain text."""
    SSML = 'ssml'
    """Data is SSML (Speech Synthesis Markup Language)."""


class SSIPClient(object):
    """Basic Speech Dispatcher client interface.

    This class provides a Python interface to Speech Dispatcher functionality
    over an SSIP connection.  The API maps directly to available SSIP commands.
    Each connection to Speech Dispatcher is represented by one instance of this
    class.
    
    Many commands take the 'scope' argument, thus it is shortly documented
    here.  It is either one of 'Scope' constants or a number of connection.  By
    specifying the connection number, you are applying the command to a
    particular connection.  This feature is only meant to be used by Speech
    Dispatcher control application, however.  More datails can be found in
    Speech Dispatcher documentation.

    """
    
    DEFAULT_HOST = '127.0.0.1'
    """Default host for server connections."""
    DEFAULT_PORT = 6560
    """Default port number for server connections."""
    DEFAULT_SOCKET_PATH = "speech-dispatcher/speechd.sock"
    """Default name of the communication unix socket"""
    
    def __init__(self, name, component='default', user='unknown', address=None,
                 autospawn=None,
                 # Deprecated ->
                 host=None, port=None, method=None, socket_path=None):
        """Initialize the instance and connect to the server.

        Arguments:
          name -- client identification string
          component -- connection identification string.  When one client opens
            multiple connections, this can be used to identify each of them.
          user -- user identification string (user name).  When multi-user
            acces is expected, this can be used to identify their connections.
          address -- server address as specified in Speech Dispatcher
            documentation (e.g. "unix:/run/user/joe/speech-dispatcher/speechd.sock"
            or "inet:192.168.0.85:6561")
          autospawn -- a flag to specify whether the library should
            try to start the server if it determines its not already
            running or not

        Deprecated arguments:
          method -- communication method to use, one of the constants defined in class
            CommunicationMethod
          socket_path -- for CommunicationMethod.UNIX_SOCKET, socket
            path in filesystem. By default, this is $XDG_RUNTIME_DIR/speech-dispatcher/speechd.sock
            where $XDG_RUNTIME_DIR is determined using the XDG Base Directory
            Specification.
          host -- for CommunicationMethod.INET_SOCKET, server hostname
            or IP address as a string.  If None, the default value is
            taken from SPEECHD_HOST environment variable (if it
            exists) or from the DEFAULT_HOST attribute of this class.
          port -- for CommunicationMethod.INET_SOCKET method, server
            port as number or None.  If None, the default value is
            taken from SPEECHD_PORT environment variable (if it
            exists) or from the DEFAULT_PORT attribute of this class.
         
        For more information on client identification strings see Speech
        Dispatcher documentation.
        """

        _home = os.path.expanduser("~")
        _runtime_dir = os.environ.get('XDG_RUNTIME_DIR', os.environ.get('XDG_CACHE_HOME', os.path.join(_home, '.cache')))
        _sock_path = os.path.join(_runtime_dir, self.DEFAULT_SOCKET_PATH)
        # Resolve connection parameters:
        connection_args = {'communication_method': CommunicationMethod.UNIX_SOCKET,
                           'socket_path': _sock_path,
                           'host': self.DEFAULT_HOST,
                           'port': self.DEFAULT_PORT,
                           }
        # Respect address method argument and SPEECHD_ADDRESS environemt variable
        _address = address or os.environ.get("SPEECHD_ADDRESS")        

        if _address:
            connection_args.update(self._connection_arguments_from_address(_address))
        # Respect the old (deprecated) key arguments and environment variables
        # TODO: Remove this section in 0.8 release
        else:
            # Read the environment variables
            env_speechd_host = os.environ.get("SPEECHD_HOST")
            try:
                env_speechd_port = int(os.environ.get("SPEECHD_PORT"))
            except:
                env_speechd_port = None
            env_speechd_socket_path = os.environ.get("SPEECHD_SOCKET")
            # Prefer old (deprecated) function arguments, but if
            # not specified and old (deprecated) environment variable
            # is set, use the value of the environment variable
            if method:
                connection_args['method'] = method
            if port:
                connection_args['port'] = port
            elif env_speechd_port:
                connection_args['port'] = env_speechd_port
            if socket_path:
                connection_args['socket_path'] = socket_path
            elif env_speechd_socket_path:
                connection_args['socket_path'] = env_speechd_socket_path
        self._connect_with_autospawn(connection_args, autospawn)
        self._initialize_connection(user, name, component)

    def _connect_with_autospawn(self, connection_args, autospawn):
        """Establish new connection (and/or autospawn server)"""
        try:
            self._conn = _SSIP_Connection(**connection_args)
        except SSIPCommunicationError as ce:
            # Suppose server might not be running, try the autospawn mechanism
            if autospawn != False:
                # Autospawn is however not guaranteed to start the server. The server
                # will decide, based on it's configuration, whether to honor the request.
                try:
                    self._server_spawn(connection_args)
                except SpawnError as se:
                    ce.set_additional_exception(se)
                    raise ce
                self._conn = _SSIP_Connection(**connection_args)
            else:
                raise

    def _initialize_connection(self, user, name, component):
        """Initialize connection -- Set client name, get id, register callbacks etc."""
        full_name = '%s:%s:%s' % (user, name, component)
        self._conn.send_command('SET', Scope.SELF, 'CLIENT_NAME', full_name)
        code, msg, data = self._conn.send_command('HISTORY', 'GET', 'CLIENT_ID')
        self._client_id = int(data[0])
        self._callback_handler = _CallbackHandler(self._client_id)
        self._conn.set_callback(self._callback_handler)
        for event in (CallbackType.INDEX_MARK,
                      CallbackType.BEGIN,
                      CallbackType.END,
                      CallbackType.CANCEL,
                      CallbackType.PAUSE,
                      CallbackType.RESUME):
            self._conn.send_command('SET', 'self', 'NOTIFICATION', event, 'on')

    def _connection_arguments_from_address(self, address):
        """Parse a Speech Dispatcher address line and return a dictionary
        of connection arguments"""
        connection_args = {}
        address_params = address.split(":")
        try:
            _method = address_params[0]
        except:
            raise SSIPCommunicationErrror("Wrong format of server address")
        connection_args['communication_method'] = _method
        if _method == CommunicationMethod.UNIX_SOCKET:
            try:
                connection_args['socket_path'] = address_params[1]
            except IndexError:
                pass # The additional parameters was not set, let's stay with defaults
        elif _method == CommunicationMethod.INET_SOCKET:
            try:
                connection_args['host'] = address_params[1]
                connection_args['port'] = int(address_params[2])
            except ValueError: # Failed conversion to int
                raise SSIPCommunicationError("Third parameter of inet_socket address must be a port number")
            except IndexError:
                pass # The additional parameters was not set, let's stay with defaults
        else:
            raise SSIPCommunicationError("Unknown communication method in address.");
        return connection_args
    
    def __del__(self):
        """Close the connection"""
        self.close()

    def _server_spawn(self, connection_args):
        """Attempts to spawn the speech-dispatcher server."""
        # Check whether we are not connecting to a remote host
        # TODO: This is a hack. inet sockets specific code should
        # belong to _SSIPConnection. We do not however have an _SSIPConnection
        # yet.
        if connection_args['communication_method'] == 'inet_socket':
            addrinfos = socket.getaddrinfo(connection_args['host'],
                                           connection_args['port'])
            # Check resolved addrinfos for presence of localhost
            ip_addresses = [addrinfo[4][0] for addrinfo in addrinfos]
            localhost=False
            for ip in ip_addresses:
                if ip.startswith("127.") or ip == "::1":
                    connection_args['host'] = ip
                    localhost=True
            if not localhost:
                # The hostname didn't resolve on localhost in neither case,
                # do not spawn server on localhost...
                raise SpawnError(
                    "Can't start server automatically (autospawn), requested address %s "
                    "resolves on %s which seems to be a remote host. You must start the "
                    "server manually or choose another connection address." % (connection_args['host'],
                                                                               str(ip_addresses),))
        cmd = os.getenv("SPEECHD_CMD")
        if not cmd:
            cmd = paths.SPD_SPAWN_CMD
        if os.path.exists(cmd):
            connection_params = []
            for param, value in connection_args.items():
                if param not in ["host",]:
                    connection_params += ["--"+param.replace("_","-"), str(value)]

            server = subprocess.Popen([cmd, "--spawn"]+connection_params,
                                      stdin=None, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
            stdout_reply, stderr_reply = server.communicate()
            retcode = server.wait()
            if retcode != 0:
                raise SpawnError("Server refused to autospawn, stating this reason: %s" % (stderr_reply,))
            return server.pid
        else:
            raise SpawnError("Can't find Speech Dispatcher spawn command %s"
                                         % (cmd))

    def set_priority(self, priority):
        """Set the priority category for the following messages.

        Arguments:
          priority -- one of the 'Priority' constants.

        """
        assert priority in (Priority.IMPORTANT, Priority.MESSAGE,
                            Priority.TEXT, Priority.NOTIFICATION,
                            Priority.PROGRESS), priority
        self._conn.send_command('SET', Scope.SELF, 'PRIORITY', priority)

    def set_data_mode(self, value):
        """Set the data mode for further speech commands.

        Arguments:
          value - one of the constants defined by the DataMode class.

        """
        if value == DataMode.SSML:
            ssip_val = 'on'
        elif value == DataMode.TEXT:
            ssip_val = 'off'
        else:
            raise ValueError(
                'Value "%s" is not one of the constants from the DataMode class.' % \
                    value)
        self._conn.send_command('SET', Scope.SELF, 'SSML_MODE', ssip_val)

    def speak(self, text, callback=None, event_types=None):
        """Say given message.

        Arguments:
          text -- message text to be spoken.  This may be either a UTF-8
            encoded byte string or a Python unicode string.
          callback -- a callback handler for asynchronous event notifications.
            A callable object (function) which accepts one positional argument
            `type' and one keyword argument `index_mark'.  See below for more
            details.
          event_types -- a tuple of event types for which the callback should
            be called.  Each item must be one of `CallbackType' constants.
            None (the default value) means to handle all event types.  This
            argument is irrelevant when `callback' is not used.

        The callback function will be called whenever one of the events occurs.
        The event type will be passed as argument.  Its value is one of the
        `CallbackType' constants.  In case of an index mark event, additional
        keyword argument `index_mark' will be passed and will contain the index
        mark identifier as specified within the text.

        The callback function should not perform anything complicated and is
        not allowed to issue any further SSIP client commands.  An attempt to
        do so would lead to a deadlock in SSIP communication.

        This method is non-blocking;  it just sends the command, given
        message is queued on the server and the method returns immediately.

        """
        self._conn.send_command('SPEAK')
        result = self._conn.send_data(text)
        if callback:
            msg_id = int(result[2][0])
            # TODO: Here we risk, that the callback arrives earlier, than we
            # add the item to `self._callback_handler'.  Such a situation will
            # lead to the callback being ignored.
            self._callback_handler.add_callback(msg_id, callback, event_types)
        return result

    def char(self, char):
        """Say given character.

        Arguments:
          char -- a character to be spoken.  Either a Python unicode string or
            a UTF-8 encoded byte string.

        This method is non-blocking;  it just sends the command, given
        message is queued on the server and the method returns immediately.

        """
        self._conn.send_command('CHAR', char.replace(' ', 'space'))
        
    def key(self, key):
        """Say given key name.

        Arguments:
          key -- the key name (as defined in SSIP); string.

        This method is non-blocking;  it just sends the command, given
        message is queued on the server and the method returns immediately.

        """
        self._conn.send_command('KEY', key)

    def sound_icon(self, sound_icon):
        """Output given sound_icon.

        Arguments:
          sound_icon -- the name of the sound icon as defined by SSIP; string.

        This method is non-blocking; it just sends the command, given message
        is queued on the server and the method returns immediately.

        """        
        self._conn.send_command('SOUND_ICON', sound_icon)
                    
    def cancel(self, scope=Scope.SELF):
        """Immediately stop speaking and discard messages in queues.

        Arguments:
          scope -- see the documentation of this class.
            
        """
        self._conn.send_command('CANCEL', scope)


    def stop(self, scope=Scope.SELF):
        """Immediately stop speaking the currently spoken message.

        Arguments:
          scope -- see the documentation of this class.
        
        """
        self._conn.send_command('STOP', scope)

    def pause(self, scope=Scope.SELF):
        """Pause speaking and postpone other messages until resume.

        This method is non-blocking.  However, speaking can continue for a
        short while even after it's called (typically to the end of the
        sentence).

        Arguments:
          scope -- see the documentation of this class.
        
        """
        self._conn.send_command('PAUSE', scope)

    def resume(self, scope=Scope.SELF):
        """Resume speaking of the currently paused messages.

        This method is non-blocking.  However, speaking can continue for a
        short while even after it's called (typically to the end of the
        sentence).

        Arguments:
          scope -- see the documentation of this class.
        
        """
        self._conn.send_command('RESUME', scope)

    def list_output_modules(self):
        """Return names of all active output modules as a tuple of strings."""
        code, msg, data = self._conn.send_command('LIST', 'OUTPUT_MODULES')
        return data

    def list_synthesis_voices(self):
        """Return names of all available voices for the current output module.

        Returns a tuple of tripplets (name, language, variant).

        'name' is a string, 'language' is an ISO 639-1 Alpha-2/3 language code
        and 'variant' is a string.  Language and variant may be None.

        """
        try:
            code, msg, data = self._conn.send_command('LIST', 'SYNTHESIS_VOICES')
        except SSIPCommandError:
            return ()
        def split(item):
            name, lang, variant = tuple(item.rsplit('\t', 3))
            return (name, lang or None, variant or None)
        return tuple([split(item) for item in data])

    def set_language(self, language, scope=Scope.SELF):
        """Switch to a particular language for further speech commands.

        Arguments:
          language -- two/three letter language code according to RFC 1766 as string, possibly with a region qualification.
          scope -- see the documentation of this class.
            
        """
        assert isinstance(language, str)
        self._conn.send_command('SET', scope, 'LANGUAGE', language)

    def get_language(self):
        """Get the current language."""
        code, msg, data = self._conn.send_command('GET', 'LANGUAGE')
        if data:
            return data[0]
        return None

    def set_output_module(self, name, scope=Scope.SELF):
        """Switch to a particular output module.

        Arguments:
          name -- module (string) as returned by 'list_output_modules()'.
          scope -- see the documentation of this class.
        
        """
        self._conn.send_command('SET', scope, 'OUTPUT_MODULE', name)

    def get_output_module(self):
        """Get the current output module."""
        code, msg, data = self._conn.send_command('GET', 'OUTPUT_MODULE')
        if data:
            return data[0]
        return None

    def set_pitch(self, value, scope=Scope.SELF):
        """Set the pitch for further speech commands.

        Arguments:
          value -- integer value within the range from -100 to 100, with 0
            corresponding to the default pitch of the current speech synthesis
            output module, lower values meaning lower pitch and higher values
            meaning higher pitch.
          scope -- see the documentation of this class.
          
        """
        assert isinstance(value, int) and -100 <= value <= 100, value
        self._conn.send_command('SET', scope, 'PITCH', value)

    def get_pitch(self):
        """Get the current pitch."""
        code, msg, data = self._conn.send_command('GET', 'PITCH')
        if data:
            return data[0]
        return None

    def set_pitch_range(self, value, scope=Scope.SELF):
        """Set the pitch range for further speech commands.

        Arguments:
          value -- integer value within the range from -100 to 100, with 0
            corresponding to the default pitch range of the current speech synthesis
            output module, lower values meaning lower pitch range and higher values
            meaning higher pitch range.
          scope -- see the documentation of this class.
          
        """
        assert isinstance(value, int) and -100 <= value <= 100, value
        self._conn.send_command('SET', scope, 'PITCH_RANGE', value)

    def set_rate(self, value, scope=Scope.SELF):
        """Set the speech rate (speed) for further speech commands.

        Arguments:
          value -- integer value within the range from -100 to 100, with 0
            corresponding to the default speech rate of the current speech
            synthesis output module, lower values meaning slower speech and
            higher values meaning faster speech.
          scope -- see the documentation of this class.
            
        """
        assert isinstance(value, int) and -100 <= value <= 100
        self._conn.send_command('SET', scope, 'RATE', value)

    def get_rate(self):
        """Get the current speech rate (speed)."""
        code, msg, data = self._conn.send_command('GET', 'RATE')
        if data:
            return data[0]
        return None

    def set_volume(self, value, scope=Scope.SELF):
        """Set the speech volume for further speech commands.

        Arguments:
          value -- integer value within the range from -100 to 100, with 100
            corresponding to the default speech volume of the current speech
            synthesis output module, lower values meaning softer speech.
          scope -- see the documentation of this class.
            
        """
        assert isinstance(value, int) and -100 <= value <= 100
        self._conn.send_command('SET', scope, 'VOLUME', value)

    def get_volume(self):
        """Get the speech volume."""
        code, msg, data = self._conn.send_command('GET', 'VOLUME')
        if data:
            return data[0]
        return None

    def set_punctuation(self, value, scope=Scope.SELF):
        """Set the punctuation pronounciation level.

        Arguments:
          value -- one of the 'PunctuationMode' constants.
          scope -- see the documentation of this class.
            
        """
        assert value in (PunctuationMode.ALL, PunctuationMode.MOST,
                         PunctuationMode.SOME, PunctuationMode.NONE), value
        self._conn.send_command('SET', scope, 'PUNCTUATION', value)

    def get_punctuation(self):
        """Get the punctuation pronounciation level."""
        code, msg, data = self._conn.send_command('GET', 'PUNCTUATION')
        if data:
            return data[0]
        return None

    def set_spelling(self, value, scope=Scope.SELF):
        """Toogle the spelling mode or on off.

        Arguments:
          value -- if 'True', all incomming messages will be spelled
            instead of being read as normal words. 'False' switches
            this behavior off.
          scope -- see the documentation of this class.
            
        """
        assert value in [True, False]
        if value == True:
            self._conn.send_command('SET', scope, 'SPELLING', "on")
        else:
            self._conn.send_command('SET', scope, 'SPELLING', "off")

    def set_cap_let_recogn(self, value, scope=Scope.SELF):
        """Set capital letter recognition mode.

        Arguments:
          value -- one of 'none', 'spell', 'icon'. None means no signalization
            of capital letters, 'spell' means capital letters will be spelled
            with a syntetic voice and 'icon' means that the capital-letter icon
            will be prepended before each capital letter.
          scope -- see the documentation of this class.
            
        """
        assert value in ("none", "spell", "icon")
        self._conn.send_command('SET', scope, 'CAP_LET_RECOGN', value)

    def set_voice(self, value, scope=Scope.SELF):
        """Set voice by a symbolic name.

        Arguments:
          value -- one of the SSIP symbolic voice names: 'MALE1' .. 'MALE3',
            'FEMALE1' ... 'FEMALE3', 'CHILD_MALE', 'CHILD_FEMALE'
          scope -- see the documentation of this class.

        Symbolic voice names are mapped to real synthesizer voices in the
        configuration of the output module.  Use the method
        'set_synthesis_voice()' if you want to work with real voices.
            
        """
        assert isinstance(value, str) and \
               value.lower() in ("male1", "male2", "male3", "female1",
                                 "female2", "female3", "child_male",
                                 "child_female")
        self._conn.send_command('SET', scope, 'VOICE_TYPE', value)

    def set_synthesis_voice(self, value, scope=Scope.SELF):
        """Set voice by its real name.

        Arguments:
          value -- voice name as returned by 'list_synthesis_voices()'
          scope -- see the documentation of this class.
            
        """
        self._conn.send_command('SET', scope, 'SYNTHESIS_VOICE', value)
        
    def set_pause_context(self, value, scope=Scope.SELF):
        """Set the amount of context when resuming a paused message.

        Arguments:
          value -- a positive or negative value meaning how many chunks of data
            after or before the pause should be read when resume() is executed.
          scope -- see the documentation of this class.
            
        """
        assert isinstance(value, int)
        self._conn.send_command('SET', scope, 'PAUSE_CONTEXT', value)

    def set_debug(self, val):
        """Switch debugging on and off. When switched on,
        debugging files will be created in the chosen destination
        (see set_debug_destination()) for Speech Dispatcher and all
        its running modules. All logging information will then be
        written into these files with maximal verbosity until switched
        off. You should always first call set_debug_destination.

        The intended use of this functionality is to switch debuging
        on for a period of time while the user will repeat the behavior
        and then send the logs to the appropriate bug-reporting place.

        Arguments:
          val -- a boolean value determining whether debugging
                 is switched on or off
          scope -- see the documentation of this class.
        
        """
        assert isinstance(val, bool)
        if val == True:
            ssip_val = "ON"
        else:
            ssip_val = "OFF"

        self._conn.send_command('SET', scope.ALL, 'DEBUG', ssip_val)


    def set_debug_destination(self, path):
        """Set debug destination.

        Arguments:
          path -- path (string) to the directory where debuging
                  files will be created
          scope -- see the documentation of this class.
        
        """
        assert isinstance(val, string)

        self._conn.send_command('SET', scope.ALL, 'DEBUG_DESTINATION', val)

    def block_begin(self):
        """Begin an SSIP block.

        See SSIP documentation for more details about blocks.

        """
        self._conn.send_command('BLOCK', 'BEGIN')

    def block_end(self):
        """Close an SSIP block.

        See SSIP documentation for more details about blocks.

        """
        self._conn.send_command('BLOCK', 'END')

    def close(self):
        """Close the connection to Speech Dispatcher."""
        if hasattr(self, '_conn'):
            self._conn.close()
            del self._conn


class Client(SSIPClient):
    """A DEPRECATED backwards-compatible API.

    This Class is provided only for backwards compatibility with the prevoius
    unofficial API.  It will be removed in future versions.  Please use either
    'SSIPClient' or 'Speaker' interface instead.  As deprecated, the API is no
    longer documented.

    """
    def __init__(self, name=None, client=None, **kwargs):
        name = name or client or 'python'
        super(Client, self).__init__(name, **kwargs)
        
    def say(self, text, priority=Priority.MESSAGE):
        self.set_priority(priority)
        self.speak(text)

    def char(self, char, priority=Priority.TEXT):
        self.set_priority(priority)
        super(Client, self).char(char)

    def key(self, key, priority=Priority.TEXT):
        self.set_priority(priority)
        super(Client, self).key(key)

    def sound_icon(self, sound_icon, priority=Priority.TEXT):
        self.set_priority(priority)
        super(Client, self).sound_icon(sound_icon)
        

class Speaker(SSIPClient):
    """Extended Speech Dispatcher Interface.

    This class provides an extended intercace to Speech Dispatcher
    functionality and tries to hide most of the lower level details of SSIP
    (such as a more sophisticated handling of blocks and priorities and
    advanced event notifications) under a more convenient API.
    
    Please note that the API is not yet stabilized and thus is subject to
    change!  Please contact the authors if you plan using it and/or if you have
    any suggestions.

    Well, in fact this class is currently not implemented at all.  It is just a
    draft.  The intention is to hide the SSIP details and provide a generic
    interface practical for screen readers.
    
    """


# Deprecated but retained for backwards compatibility

# This class was introduced in 0.7 but later renamed to CommunicationMethod
class ConnectionMethod(object):
    """Constants describing the possible methods of connection to server.

    Retained for backwards compatibility but DEPRECATED. See CommunicationMethod."""
    UNIX_SOCKET = 'unix_socket'
    """Unix socket communication using a filesystem path"""
    INET_SOCKET = 'inet_socket'
    """Inet socket communication using a host and port"""

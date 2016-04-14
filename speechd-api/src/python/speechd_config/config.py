# Copyright (C) 2008 Brailcom, o.p.s.
#
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software
# Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA

import sys
import os
import shutil
import fileinput
import socket
import time
import datetime

from optparse import OptionParser

# Configuration and sound data paths
import paths

def report(msg):
    """Output information messages for the user on stdout
    and if desired, by espeak synthesis"""
    print msg
    if options.use_espeak_synthesis:
        os.system("espeak \"" + msg + "\"")

def input_audio_icon():
    """Produce a sound for the event 'input requested' used in question()"""
    if options.use_espeak_synthesis:
        os.system("espeak \"Type in\"");

def question(text, default):
    """Ask a simple question and suggest the default value"""

    while 1:
        if isinstance(default, bool):
            if default == True:
                default_str = "yes"
            else:
                default_str = "no"
        else:
            default_str = str(default)
        report(text + " ["+default_str+"] :")
        input_audio_icon()

        if not options.dont_ask:
            str_inp = raw_input(">") 

        # On plain denter, return default
        if options.dont_ask or (len(str_inp) == 0):
            return default
        # If a value was typed, check it and convert it
        elif isinstance(default, bool):
            if str_inp in ["yes","y", "Y", "true","t", "1"]:
                return True;
            elif str_inp in ["no", "n", "N", "false", "f", "0"]:
                return False;
            else:
                report ("Unknown answer (type 'yes' or 'no')")
                continue
        elif isinstance(default, int):
            return int(str_inp)
        elif isinstance(default, str):
            return str_inp

def question_with_suggested_answers(text, default, suggest):
    """Ask a question with suggested answers. If the answer typed is not
    in 'suggest', the user is notified and given an opportunity to correct
    his choice"""
    
    reply = question(text, default)
    while reply not in suggest:
        report("""The value you have chosen is not among the suggested values.
You have chosen '%s'.""" % reply)
        report("The suggested values are " + str(suggest))
        correct = question("Do you want to correct your answer?", True)
        if correct == True:
            reply = question(text, default)
        else:
            return reply
    return reply

def question_with_required_answers(text, default, required):
    """Ask a question and repeat it until the answer typed in is in 'required'"""
    
    reply = question(text, default)
    while reply not in required:
        report("You have chosen '%s'. Please choose one of %s" % (reply, str(required)))
        reply = question(text, default)
    return reply

class Options(object):
    """Configuration for spdconf"""

    _conf_options = \
        {
        'create_user_configuration':
            {
            'descr' : "Create Speech Dispatcher configuration for the given user",
            'doc' : None,
            'type' : bool,
            'default' : False,
            'command_line' : ('-u', '--create-user-conf'),
            },
        'config_basic_settings_user':
            {
            'descr' : "Configure basic settings in user configuration",
            'doc' : None,
            'type' : bool,
            'default' : False,
            'command_line' : ('-c', '--config-basic-settings-user'),
            },
        'config_basic_settings_system':
            {
            'descr' : "Configure basic settings in system-wide configuration",
            'doc' : None,
            'type' : bool,
            'default' : False,
            'command_line' : ('-C', '--config-basic-settings-system'),
            },
        'diagnostics':
            {
            'descr' : "Diagnose problems with the current setup",
            'doc' : None,
            'type' : bool,
            'default' : False,
            'command_line' : ('-d', '--diagnostics'),
            },
        'test_spd_say':
            {
            'descr' : "Test connection to Speech Dispatcher using spd-say",
            'doc' : None,
            'type' : bool,
            'default' : False,
            'command_line' : ('-s', '--test-spd-say'),
            },
        'test_festival':
            {
            'descr' : "Test whether Festival works as a server",
            'doc' : None,
            'type' : bool,
            'default' : False,
            'command_line' : ('', '--test-festival'),
            },
        'test_espeak':
            {
            'descr' : "Test whether Espeak works as a standalone binary",
            'doc' : None,
            'type' : bool,
            'default' : False,
            'command_line' : ('', '--test-espeak'),
            },
        'test_alsa':
            {
            'descr' : "Test ALSA audio",
            'doc' : None,
            'type' : bool,
            'default' : False,
            'command_line' : ('', '--test-alsa'),
            },

        'test_pulse':
            {
            'descr' : "Test Pulse Audio",
            'doc' : None,
            'type' : bool,
            'default' : False,
            'command_line' : ('', '--test-pulse'),
            },

        'use_espeak_synthesis':
            {
            'descr' : "Use espeak to synthesize messages",
            'doc' : None,
            'type' : bool,
            'default' : False,
            'command_line' : ('-e', '--espeak'),
            },
        'dont_ask':
            {
            'descr' : "Do not ask any questions, allways use default values",
            'doc' : None,
            'type' : bool,
            'default' : False,
            'command_line' : ('-n', '--dont-ask'),
            },
        'debug':
            {
            'descr' : "Debug a problem and generate a report",
            'doc' : None,
            'type' : bool,
            'default' : False,
            'command_line' : ('-D', '--debug'),
            },
        }

    def __init__(self):
        usage = """A simple tool for basic configuration of Speech Dispatcher
and problem diagnostics

usage: %prog [options]"""
        self.cmdline_parser = OptionParser(usage)

        for option, definition in self._conf_options.iteritems():
            # Set object attributes to default values
            def_val = definition.get('default', None)
            setattr(self, option, def_val)
            
            # Fill in the cmdline_parser object
            if definition.has_key('command_line'):
                descr = definition.get('descr', None)                
                type = definition.get('type', None)
                
                if definition.has_key('arg_map'):
                    type, map = definition['arg_map']
                if type == str:
                    type_str = 'string'
                elif type == int:
                    type_str = 'int'
                elif type == float:
                    type_str = 'float'
                elif type == bool:
                    type_str = None
                else:
                    raise "Unknown type"
                
                if type != bool:
                    self.cmdline_parser.add_option(type=type_str, dest=option,
                                                   help=descr,
                                                   *definition['command_line'])
                else: # type == bool
                    self.cmdline_parser.add_option(action="store_true", dest=option,
                                                   help=descr,
                                                   *definition['command_line'])
            
        # Set options according to command line flags
        (cmdline_options, args) = self.cmdline_parser.parse_args()

        for option, definition in self._conf_options.iteritems():
                val = getattr(cmdline_options, option, None)
                if val != None:
                    if definition.has_key('arg_map'):
                        former_type, map = definition['arg_map']
                        try:
                            val = map[val]
                        except KeyError:
                            raise "Invalid option value: "  + str(val)
                        
                    setattr(self, option, val)
        
        #if len(args) != 0:
           # raise "This command takes no positional arguments (without - or -- prefix)"

class Tests:
    """Tests of functionality of Speech Dispatcher and its dependencies
    and methods for determination of proper paths"""

    def default_port(self):
        if os.environ.has_key('SPEECHD_PORT'):
            port = int(os.environ['SPEECHD_PORT'])
            report("""
Speech Dispatcher port suggested by SPEECHD_PORT environment variable: %d""" %
                   port)
            return port
        else:
            return 6560

    def user_speechd_dir(self):
        """Return user Speech Dispatcher configuration and logging directory"""
        return os.path.expanduser(os.path.join('~', '.speech-dispatcher'))

    def user_speechd_dir_exists(self):
        """Determine whether user speechd directory exists"""
        return os.path.exists(self.user_speechd_dir())

    def user_conf_dir(self):
        """Return user configuration directory"""
        return os.path.join(self.user_speechd_dir(), "conf")

    def system_conf_dir(self):
        """Determine system configuration directory"""
        return paths.SPD_CONF_PATH;

    def user_conf_dir_exists(self):
        """Determine whether user configuration directory exists"""
        return os.path.exists(self.user_conf_dir())

    def festival_connect(self, host="localhost", port=1314):
        """
        Try to connect to festival and determine whether it is possible.
        On success self.festival_socket is initialized with the openned socket.
        """
        self.festival_socket = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
        try:
            self.festival_socket.connect((socket.gethostbyname(host), port))
        except socket.error, (num, reson):
            report("""ERROR: It was not possible to connect to Festival on the
given host and port. Connection failed with error %d : %s .""" % (num, reson))
            report("""Hint: Most likely, your Festival server is not running now
(or not at the default port.
Try /etc/init.d/festival start or run 'festival --server' from the command line.""")
            return False

        return True
        
    def festival_with_freebsoft_utils(self):
        """Test whether festival contains working festival-freebsoft-utils.
        This test must be performed only after festival_connect().
        """
        self.festival_socket.send("(require 'speech-dispatcher)\n");
        reply = self.festival_socket.recv(1024);
        if "LP" in reply:
            return True
        else:
            report("""ERROR: Your Festival server is working but it doesn't seem
to load festival-freebsoft-utils. You need to install festival-freebsoft-utils
to be able to use Festival with Speech Dispatcher.""");
            return False

    def python_speechd_in_path(self):
        """Try whether python speechd library is importable"""
        try:
            import speechd
        except:
            report("""Python can't find the Speech Dispatcher library.
Is it installed? This won't prevent Speech Dispatcher to work, but no
Python applications like Orca will find it.
Search for package like python-speechd.""")
            return False
        return True

    def audio_try_play(self, type):
        """Try to play a sound through the standard playback utility for the
        given audio method."""
        wavfile = os.path.join(paths.SPD_SOUND_DATA_PATH,"test.wav")

        if type == 'alsa':
            cmd = "aplay" + " " + wavfile
        elif type == 'pulse':
            cmd = "paplay" + " " + wavfile
        elif type == 'oss':
            raise NotImplementedError

        try:
            ret = os.system(cmd)
        except:
            report("""Can't execute the %s command, The tool might just not be installed.
This doesn't mean that the sound system is not working, but we can't determine it.
Please test manually.""")
            reply = question("Are you sure that %s audio is working?" % type, False)
            return reply
    
        if ret:
            report("Can't play audio via %s" % cmd)
            report("""Your audio doesn't seem to work, please fix audio first or choose
a different method.""")
            return False


        reply = question("Did you hear the sound?", True)
        
        if not reply:
            report("""Please examine the above output from the sound playback
utility. If everything seems right, are you sure your audio is loud enough and
not muted in the mixer? Please fix your audio system first or choose a different
audio output method in configuration.""")
            return False

    def test_spd_say(self, host="localhost", port=None):
        """Test Speech Dispatcher using spd_say"""

        report("Testing Speech Dispatcher using spd_say")
        
        while True:
            try:
                if port:
                    ret = os.system("SPEECHD_PORT=%d spd-say -P important \"Speech Dispatcher seems to work\""
                                    % port)
                else:
                    ret = os.system("spd-say -P important \"Speech Dispatcher seems to work\"")
            except:
                report("""Can't execute the spd-say binary,
it is likely that Speech Dispatcher is not installed.""")
                return False
            if ret:
                report("Can't connect to server on host %s port %d" % (host, port))
                scan = question("Do you want to scan nearby ports for a running Speech Dispatcher?",
                                True)
                if scan:
                    speechd_ports = []
                    for sport in range (port-10, port+10):
                        ret = os.system(
                            "SPEECHD_PORT=%d spd-say \"Speech Dispatcher seems to work on port %d\""
                                        % (sport, sport))
                        if not ret:
                            report("Speech Dispatcher running on port %d" % sport)
                            speechd_ports += [sport]
                    if len(speechd_ports) == 0:
                        report("No Speech Dispatchers found on ports %d to %d" % (port-10, port+10))
                        report("""
Speech Dispatcher doesn't seem to be running. Please start it and try again.""")
                    else:
                        report("""Speech Dispatchers active on ports: %s.
This very likely means a slight misconfiguration in your environment.
You can influence the default Speech Dispatcher port through the SPEECHD_PORT
environment variable. You can try to run this test again as
$ SPEECHD_PORT=%d spd-conf -d
""" % (str(speechd_ports), speechd_ports[0]))
                return False

            hearing_test = question("Did you hear the message about Speech Dispatcher working?",
                                    True)
            if hearing_test:
                report("Speech Dispatcher is working")
                return True
            else:
                repeat = question("Do you want to repeat the test?", True)
                if repeat:
                    continue
                else:
                    report("Speech Dispatcher not working now")
                    return False

    def test_festival(self):
        """Test whether Festival works as a server"""
        report("Testing whether Festival works as a server")

        ret = self.festival_connect()
        while not ret:
            reply = question("Do you want to try again?", True)
            if not reply:
                report("Festival server is not working now.")
                return False
        report("Festival server seems to work correctly")
        return True

    def test_espeak(self):
        """Test the espeak utility"""

        report("Testing whether Espeak works")
        
        while True:
            try:
                os.system("espeak \"Espeak seems to work\"")
            except:
                report("""Can't execute the espeak binary, it is likely that espeak
is not installed.""")
                return False

            hearing_test = question("Did you hear the message 'Espeak seems to work'?",
                                    True)
            if hearing_test:
                report("Espeak is working")
                return True
            else:
                repeat = question("Do you want to repeat the test?", True)
                if repeat:
                    continue
                else:
                    report("""
Espeak is installed, but the espeak utility is not working now.
This doesn't necessarily mean that your espeak won't work with
Speech Dispatcher.""")
                    return False

    def test_alsa(self):
        """Test ALSA sound output"""
        report("Testing ALSA sound output")
        return self.audio_try_play(type='alsa')

    def test_pulse(self):
        """Test Pulse Audio sound output"""
        report("Testing PULSE sound output")
        return self.audio_try_play(type='pulse')

    def diagnostics(self, speechd_port = 6560,
                    speechd_running = True,
                    output_modules=[], audio_output=[]):

        """Perform a complete diagnostics"""
        working_modules = []
        working_audio = []

        if speechd_running:
            # Test whether Speech Dispatcher works
            if self.test_spd_say(port=speechd_port):
                dispatcher_working = True
                skip = question("Speech Dispatcher works. Do you want to skip other tests?",
                                True)
                if skip:
                    return {'dispatcher_working': True}
            else:
                dispatcher_working = False
        else:
            dispatcher_working = False

        if not dispatcher_working:
            if not question("""
Speech Dispatcher isn't running or is running on a different port (see above),
do you want to proceed with other tests? (They can help to determine
what is wrong)""", True):
                return {'dispatcher_working': False}


        def decide_to_test(identifier, name, listing):
            """"Ask the user whether to test a specific capability"""
            if ((identifier in listing)
                or (not len(listing)
                    and question("Do you want to test the %s now?" % name, True))):
                return True
            else:
                return False

        if decide_to_test('festival', "Festival synthesizer", output_modules): 
            if self.test_festival():
                working_modules += ["festival"]

        if decide_to_test('espeak', "Espeak synthesizer", output_modules): 
            if not self.test_espeak():
                working_modules += ["espeak"]

        if decide_to_test('alsa', "ALSA sound system", audio_output): 
            if not self.test_alsa():
                working_audio += ["alsa"]

        if decide_to_test('pulse', "Pulse Audio sound system", audio_output): 
            if not self.test_pulse():
                working_audio += ["pulse"]

        report("Testing whether Python Speech Dispatcher library is in path and importable")
        python_speechd_working = test.python_speechd_in_path()
        
        # TODO: Return results of the diagnostics
        return  {'dispatcher_working': dispatcher_working,
                 'audio': working_audio,
                 'synthesizers': working_modules,
                 'python_speechd' : python_speechd_working}

    def write_diagnostics_results(self, results):
        """Write out diagnostics results using report()"""

        report("""

Diagnostics results:""")
        if results.has_key('dispatcher_working'):
            if results['dispatcher_working']:
                report("Speech Dispatcher is working")
            else:
                report("Speech Dispatcher not working");
        if results.has_key('synthesizers'):
            report("Synthesizers that were tested and work: %s" %
                   str(results['synthesizers']))
        if results.has_key('audio'):
            report("Audio systems that were tested and work: %s" %
                   str(results['audio']))
        if results.has_key('python_speechd'):
            if(results['python_speechd']):
                report("Python Speech Dispatcher module is importable")
            else:
                report("""Python Speech Dispatcher module not importable.
Either not installed or not in path.""")
        report("End of diagnostics results")

    def user_configuration_seems_complete(self):
        """Decide if the user configuration seems reasonably complete"""
        if not os.path.exists(os.path.join(self.user_conf_dir(), "speechd.conf")):
            return False

        if not len(os.listdir(self.user_conf_dir())) > 2:
            return False

        if not os.path.exists(os.path.join(self.user_conf_dir(), "modules")):
            return False

        if not os.path.exists(os.path.join(self.user_conf_dir(), "clients")):
            return False

        return True

    def debug_and_report(self, speechd_port=6560, type = None):
        """Start Speech Dispatcher in debugging mode, collect the debugging output
        and offer to send it to the developers"""

        report("Starting collecting debugging output, configuration and logfiles");

        if not type:
            type = question_with_required_answers("""
Do you want to debug 'system' or  'user' Speech Dispatcher?""",
                                                  'user', ['user', 'system'])

        # Try to kill running Speech Dispatcher
        reply = question("""It is necessary to kill the currently running Speech Dispatcher
processes. Do you want to do it now?""", True);
        if reply:
            os.system("killall speech-dispatcher")
        else:
            report("""
You decided not to kill running Speech Dispatcher processes.
Please make sure your Speech Dispatcher is not running now.""")
            reply = question("Is your Speech Dispatcher not running now?", True);
            if not reply:
                report("Can't continue, please stop your Speech Dispatcher and try again")


        time.sleep(2)


        # All debugging files are written to TMPDIR/speech-dispatcher/
        if os.environ.has_key('TMPDIR'):
            tmpdir = os.environ['TMPDIR']
        else:
            tmpdir = "/tmp/"
        debugdir_path = os.path.join(tmpdir, "speechd-debug")
        date = datetime.date.today()
        debugarchive_path = os.path.join(tmpdir, "speechd-debug-%d-%d-%d.tar.gz" %
                                         (date.day, date.month, date.year))

        # Start Speech Dispatcher with debugging enabled
        if type == 'user':
            report("Speech Dispatcher will be started now in debugging mode")
            speechd_started = not os.system("speech-dispatcher -D")
            configure_directory = test.user_conf_dir()
        else:
            report("Warning: You must be root or under sudo to do this.")        
            report("""
Please start your system Speech Dispatcher now with parameter '-D'""");
            reply = question("Is your Speech Dispatcher running now?", True);
            if reply:
                speechd_started = True
            else:
                report("Can't continue");
            configure_directory = test.system_conf_dir()
        time.sleep(2)

        if not speechd_started:
            reply = question("Speech Dispatcher failed to start, continuing anyway");

        report("Trying to speak some messages");
        ret = 1
        while ret:
            ret = os.system("SPEECHD_PORT=%d spd-say \"Speech Dispatcher debugging 1\""
                            % speechd_port)
            if ret:
                reply = question("""
Can't connect Speech Dispatcher on the given port.
Do you want to try another port?""", True)
                if reply:
                    speechd_port = question("What port?", 6561)
                else:
                    report("Can't test Speech Dispatcher connection, continuing anyway.")
                    
        os.system("SPEECHD_PORT=%d spd-say \"Speech Dispatcher debugging 2\""
                  % speechd_port)
        os.system("SPEECHD_PORT=%d spd-say \"Speech Dispatcher debugging 3\""
                  % speechd_port)

        report("Please wait (about 5 seconds)")
        time.sleep(5)

        report("Collecting debugging output and your configuration information") 

        os.system("umask 077");
        os.system("tar -cz %s %s > %s" % 
                  (debugdir_path, configure_directory, debugarchive_path))   
        os.system("killall speech-dispatcher")
        os.system("rm -rf %s" % debugdir_path)

        report("""
Please send %s to speechd@bugs.freebsoft.org with
a short description of what you did. We will get in touch with you soon
and suggest a solution.""" % debugarchive_path)
        
test = Tests()

class Configure:

    """Setup user configuration and/or set basic options in user/system configuration"""

    default_output_module = None
    default_language = None
    default_audio_method = None
    port = None
    
    def remove_user_configuration(self):
        """Remove user configuration tree"""
        shutil.rmtree(test.user_conf_dir())

    def options_substitute(self, configfile, options):
        """Substitute the given options with given values.

        Arguments:
        configfile -- the file path of the configuration file as a string
        options -- a list of tuples (option_name, value)"""

        # Parse config file in-place and replace the desired options+values
        for line in fileinput.input(configfile, inplace=True, backup=".bak"):
            # Check if the current line contains any of the desired options
            for opt, value in options.iteritems():
                if opt in line:
                    # Now count unknown words and try to judge if this is
                    # real configuration or just a comment
                    unknown = 0
                    for word in line.split():
                        if word =='#' or word == '\t':
                            continue
                        elif word == opt:
                            # If a foreign word went before our option identifier,
                            # we are not in code but in comments
                            if unknown != 0:
                                unknown = 2;
                                break;
                        else:
                            unknown += 1

                    # Only consider the line as the actual code when the keyword
                    # is followed by exactly one word value. Otherwise consider this
                    # line as plain comment and leave intact
                    if unknown == 1:
                        # Convert value into string representation in spd_val
                        if isinstance(value, bool):
                            if value == True:
                                spd_val = "1"
                            elif value == False:
                                spd_val = "2"
                        elif isinstance(value, int):
                            spd_val = str(value)
                        else:
                            spd_val = str(value)
                            
                        print opt + "   " + spd_val
                        break
            
            else:
                print line,
                
    def create_user_configuration(self):
        """Create user configuration in the standard location"""

        # Ask before touching things that we do not have to!
        if test.user_speechd_dir_exists():
            if test.user_conf_dir_exists():
                if test.user_configuration_seems_complete():
                    reply = question(
                        """User configuration already exists.
Do you want to rewrite it with a new one?""", False)
                    if reply == False:
                        report("Keeping configuration intact and continuing with settings.")
                        return
                    else:
                        self.remove_user_configuration()
                else:
                    reply = question(
                        """User configuration already exists, but it seems to be incomplete.
Do you want to keep it?""", False)
                    if reply == False:
                        self.remove_user_configuration()
                    else:
                        report("Keeping configuration intact and aborting.")
                        return

            # TODO: Check for permissions on logfiles and pid
        else:
            report("Creating " + test.user_speechd_dir());
            os.mkdir(test.user_speechd_dir())

        # Copy the original intact configuration files
        # creating a conf/ subdirectory
        shutil.copytree(paths.SPD_CONF_ORIG_PATH, test.user_conf_dir())
        
        report("User configuration created in %s" % test.user_conf_dir())

    def configure_basic_settings(self, type='user'):
        """Ask for basic settings and rewrite them in the configuration file"""

        if type == 'user':
            report("Configuring user settings for Speech Dispatcher");
        elif type == 'system':
            report("Warning: You must be root or under sudo to do this.")
            report("Configuring system settings for Speech Dispatcher");
        else:
            assert(0);

        # Now determine the most important config option
        self.default_output_module = question_with_suggested_answers(
            "Default output module",
            "espeak",
            ["espeak", "flite", "festival", "cicero", "ibmtts", "ekho"])

        self.default_language = question(
            "Default language (two-letter iso language code like \"en\" or \"cs\")",
            "en")

        self.default_audio_method = question_with_suggested_answers(
            "Default audio output method",
            "alsa",
            ["alsa", "pulse", "oss"])
    
        if type == 'user':
            default_port = 6561
        else:
            default_port = 6560
        self.port = question(
            "Default port",
            default_port)

        # Substitute given configuration options
        if type == 'user':
            configfile = os.path.join(test.user_conf_dir(), "speechd.conf")
        elif type == 'system':
            configfile = os.path.join(test.system_conf_dir(), "speechd.conf")

        self.options_substitute(configfile, 
                                {"DefaultModule": self.default_output_module,
                                 "DefaultLanguage": self.default_language,
                                 "AudioOutputMethod": self.default_audio_method,
                                 "Port": self.port})
        if type == 'user':
            self.setup_port = question(
                "Do you want to configure your system to use Speech Dispatcher on this port in ~/.profile?",
                True)
            self.setup_autostart = question(
                "Do you want to have Speech Dispatcher automatically started from ~/.config/autostart ?",
                True)

            if self.setup_port:
                os.system("""echo "export SPEECHD_PORT=%d" >> ~/.profile""" % self.port) 
            if self.setup_autostart:
                os.system("""cp %s ~/.config/autostart/""" % os.path.join(paths.SPD_DESKTOP_CONF_PATH,
                                                                          "speechd.desktop")) 
                         
                report("""
Configuration written to %s
Basic configuration now complete. You might still need to fine tune it by
manually editing the configuration above file. Especially if you need to
use special audio settings, non-standard synthesizer ports etc.""" % configfile)

    def speechd_start_user(self):
        """Start Speech Dispatcher in user-mode"""

        report("Starting Speech Dispatcher in user-mode")

        not_started = True
        while not_started:
            not_started = os.system("speech-dispatcher");
            if not_started:
                report("Can't start Speech Dispatcher. Exited with status %d" %
                       not_started)
                reply = question("""
Perhaps this is because your Speech Dispatcher is already running.
Do you want to kill all running Speech Dispatchers and try again?""", True)
                if reply:
                    os.system("killall speech-dispatcher")
                else:
                    report("Can't start Speech Dispatcher");
                    return False
        return True

    def speechd_start_system(self):
        """Start Speech Dispatcher in system-mode"""

        report("Warning: You must be root or under sudo to do this.")        
        report("Starting Speech Dispatcher in system-mode")
        
        reply = question("Is your system using an /etc/init.d/speech-dispatcher script?",
                         True)
        if reply:
            report("Stopping Speech Dispatcher in case any is running already")
            os.system("/etc/init.d/speech-dispatcher stop") 
            report("Starting Speech Dispatcher via /etc/init.d/speech-dispatcher")
            ret = os.system("/etc/init.d/speech-dispatcher start")
            if ret:
                report("Can't start Speech Dispatcher. Exited with status %d" % ret)
                return False
        else:
            report("""Do not know how to start system Speech Dispatcher,
you have to start it manually to continue.""")
            reply = question("Have you started Speech Dispatcher now?", True)
            if not reply:
                report("Can't continue")
                return False
        return True

    def complete_config(self):
        """Create a complete configuration, run diagnosis and if necessary, debugging"""

        speechd_type = question_with_required_answers(
            "Do you want to create/setup a 'user' or 'system' configuration",
            'user', ['user', 'system'])

        if speechd_type == 'user':
            self.create_user_configuration()
            self.configure_basic_settings(type='user')
        elif speechd_type == 'system':
            self.configure_basic_settings(type='system')
        else:
            assert(False)

        reply = question("Do you want to start/restart Speech Dispatcher now and run some tests?", True)
        if not reply:
            report("Your configuration is now done but not tested")
            return
        else:
            if speechd_type == 'user':
                started = self.speechd_start_user()
            elif speechd_type == 'system':
                started= self.speechd_start_system()

        if not started:
            report("Your Speech Dispatcher is not running now on port %d" %
                   self.port);
            
        result = test.diagnostics(speechd_port=self.port,
                                  speechd_running = started,
                                  audio_output=[self.default_audio_method],
                                  output_modules=[self.default_output_module]);
        test.write_diagnostics_results(result)

        if result['dispatcher_working']:
            if self.port != test.default_port():
                report("""
Warning: Speech Dispatcher is configured for port %d
but this is not the default port on your system (%d).
You must configure your SPEECHD_CONF environment variable
for this value or adjust the settings in the particular clients.
If you opted to do that during the run of this script, you just
have to restart your GNOME now.
"""  % (self.port, test.default_port()))
                
        else:
            reply = question("Do you want to run debugging now and send a request for help to the developers?",
                             True)
            if reply:
                test.debug_and_report(speechd_port=self.port, type=speechd_type)


# Basic objects
options = Options()
configure = Configure()
test = Tests()


def main():

    report("Speech Dispatcher configuration tool")
    
    if options.create_user_configuration:
        configure.create_user_configuration()
        reply = question("Do you want to continue with basic settings?", True)
        if reply:
            configure.configure_basic_settings(type='user')
    elif options.config_basic_settings_user:
        configure.configure_basic_settings(type='user')

    elif options.config_basic_settings_system:
        configure.configure_basic_settings(type='system')

    elif options.test_festival:
        test.test_festival()

    elif options.test_spd_say:
        test.test_spd_say(port=test.default_port())

    elif options.test_espeak:
        test.test_espeak()

    elif options.test_alsa:
        test.audio_try_play(type='alsa')

    elif options.test_pulse:
        test.audio_try_play(type='pulse')

    elif options.diagnostics:
        ret = test.diagnostics(test.default_port())
        test.write_diagnostics_results(ret)

    elif options.debug:
        test.debug_and_report(test.default_port())

    else:
        reply = question("Do you want to setup a completely new configuration?", True)
        if reply:
            configure.complete_config()
        else:
            reply = question("Do you want to run diagnosis of problems?", True)
            if reply:
                test.diagnostics(test.default_port())
                test.write_diagnostics_results(ret)
            else:
                report("""Please run this command again and select what you want to do
or read the quick help available through '-h' or '--help'.""")

if __name__ == "__main__":
    sys.exit(main())
else:
    main()

#!/usr/bin/env python

# Copyright (C) 2003, 2006, 2007 Brailcom, o.p.s.
#
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public Licensex1 as published by
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

import unittest
import time

from client import PunctuationMode, CallbackType, SSIPClient, Scope, Speaker


class _SSIPClientTest(unittest.TestCase):
        
    def setUp(self):
        self._client = SSIPClient('test')
        self._client.set_language('en')
        self._client.set_rate(30)

    def tearDown(self):
        self._client.close()

class AutomaticTest(_SSIPClientTest):
    """A set of tests which may be evaluated automatically.

    Please put all tests which require a user to listen to their output to the
    VoiceTest below.

    """
    def test_callbacks(self):
        # TODO: This needs to be fixed. There is no guarantee that
        # the message will start in one second nor is there any
        # guarantee that it will start at all. It can be interrupted
        # by other applications etc. Also there is no guarantee that
        # the cancel will arrive on time and the end callback will be
        # received on time. Also the combination cancel/end does not have
        # to work as expected and SD and the interface can still be ok.
        # -- Hynek Hanke
        self._client.set_output_module('flite')
        called = {CallbackType.BEGIN: [],
                  CallbackType.CANCEL: [],
                  CallbackType.END: []}
        self._client.speak("This message should get interrupted.  It is "
                           "hopefully long enough to last more than 1 second.",
                           callback=lambda type: called[type].append('msg1'))
        self._client.speak("This second message should not be spoken at all.",
                           callback=lambda type: called[type].append('msg2'))
        time.sleep(1)
        self._client.cancel()
        self._client.speak("Hi.",
                           callback=lambda type: called[type].append('msg3'))
        # Wait for pending events...
        time.sleep(3)
        started, canceled, ended = [called[t] for t in (CallbackType.BEGIN,
                                                        CallbackType.CANCEL,
                                                        CallbackType.END)]
        assert started == ['msg1', 'msg3'] and ended == ['msg3'] and \
               'msg1' in canceled and 'msg2' in canceled and \
               'msg3' not in canceled, \
               (called,
                "This failure only indicates a possible error.  The test "
                "depends on proper timing and results may warry depending "
                "on the used output module and other conditions.  See the "
                "code of this test method if you want to investigate "
                "further.")

    

class VoiceTest(_SSIPClientTest):
    """This set of tests requires a user to listen to it.

    The success or failure of the tests defined here can not be detected
    automatically.

    """
    
    def test_escapes(self):
        c = self._client
        c.speak("Testing data escapes:")
        c.set_punctuation(PunctuationMode.ALL)
        c.speak(".")
        c.speak("Marker at the end.\r\n.\r\n")
        c.speak(".\r\nMarker at the beginning.")
    
    def test_voice_properties(self):
        c = self._client
        c.speak("Testing voice properties:")
        c.set_pitch(-100)
        c.speak("I am fat Billy")
        c.set_pitch(100)
        c.speak("I am slim Willy")
        c.set_pitch(0)
        c.set_rate(100)
        c.speak("I am quick Dick.")
        c.set_rate(-80)
        c.speak("I am slow Joe.")
        c.set_rate(0)
        c.set_pitch(100)
        c.set_volume(-50)
        c.speak("I am quiet Mariette.")
        c.set_volume(100)
        c.speak("I am noisy Daisy.")

    def test_other_commands(self):
        c = self._client
        c.speak("Testing other commands:")
        c.char("a")
        c.key("shift_b")
        c.sound_icon("empty")
        
    def test_lists(self):
         c = self._client
         for module in  c.list_output_modules():
             c.set_output_module(module)
             print "**", module
             c.speak(module +"using default voice")
             for name, lang, dialect in c.list_synthesis_voices():
                 print " -", module, name, lang, dialect
                 c.set_synthesis_voice(name)
                 c.speak(module +" using voice "+ name)
        

if __name__ == '__main__':
    unittest.main()

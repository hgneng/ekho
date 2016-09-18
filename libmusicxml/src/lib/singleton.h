/*
  MusicXML Library
  Copyright (C) Grame 2006-2013

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.

  Grame Research Laboratory, 11, cours de Verdun Gensoul 69002 Lyon - France
  research@grame.fr
*/

#ifndef __singleton__
#define __singleton__

// warning: this is NOT a thread safe implementation
template <typename T> class singleton {
	public:
		static T& instance () {
			static T theSingleInstance;
			return theSingleInstance;
		}
};


#endif

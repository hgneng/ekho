/*
  MusicXML Library
  Copyright (C) Grame 2006-2013

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.

  Grame Research Laboratory, 11, cours de Verdun Gensoul 69002 Lyon - France
  research@grame.fr
*/

#ifndef __sortVisitor__
#define __sortVisitor__

#include "visitor.h"
#include "typedefs.h"
#include "xml.h"

namespace MusicXML2 
{   

/*!
\addtogroup visitors
@{
*/

/*!
\brief A visitor that sorts a musicxml tree according to the dtd
*/
class sortvisitor : 
	public visitor<S_accord>,
	public visitor<S_accordion_registration>,
	public visitor<S_appearance>,
	public visitor<S_attributes>,
	public visitor<S_backup>,
	public visitor<S_barline>,
	public visitor<S_bass>,
	public visitor<S_beat_repeat>,
	public visitor<S_bend>,
	public visitor<S_clef>,
	public visitor<S_defaults>,
	public visitor<S_degree>,
	public visitor<S_direction>,
	public visitor<S_figure>,
	public visitor<S_figured_bass>,
	public visitor<S_forward>,
	public visitor<S_frame_note>,
	public visitor<S_frame>,
	public visitor<S_harmonic>,
	public visitor<S_harmony>,
	public visitor<S_identification>,
	public visitor<S_measure_style>,
	public visitor<S_metronome_note>,
	public visitor<S_metronome_tuplet>,
	public visitor<S_midi_instrument>,
	public visitor<S_notations>,
	public visitor<S_note>,
	public visitor<S_page_layout>,
	public visitor<S_page_margins>,
	public visitor<S_part_group>,
	public visitor<S_pedal_tuning>,
	public visitor<S_pitch>,
	public visitor<S_print>,
	public visitor<S_rest>,
	public visitor<S_root>,
	public visitor<S_scaling>,
	public visitor<S_score_instrument>,
	public visitor<S_score_part>,
	public visitor<S_score_partwise>,
	public visitor<S_slash>,
	public visitor<S_sound>,
	public visitor<S_staff_details>,
	public visitor<S_staff_tuning>,
	public visitor<S_system_layout>,
	public visitor<S_system_margins>,
	public visitor<S_time_modification>,
	public visitor<S_transpose>,
	public visitor<S_tuplet_actual>,
	public visitor<S_tuplet_normal>,
	public visitor<S_tuplet>,
	public visitor<S_unpitched>,
	public visitor<S_work>
{
	protected:
 
	public:
				 sortvisitor();
       	virtual ~sortvisitor() {}
              
		virtual void visitStart( S_accord& elt );
		virtual void visitStart( S_accordion_registration& elt );
		virtual void visitStart( S_appearance& elt );
		virtual void visitStart( S_attributes& elt );
		virtual void visitStart( S_backup& elt );
		virtual void visitStart( S_barline& elt );
		virtual void visitStart( S_bass& elt );
		virtual void visitStart( S_beat_repeat& elt );
		virtual void visitStart( S_bend& elt );
		virtual void visitStart( S_clef& elt );
		virtual void visitStart( S_defaults& elt );
		virtual void visitStart( S_degree& elt );
		virtual void visitStart( S_direction& elt );
		virtual void visitStart( S_figure& elt );
		virtual void visitStart( S_figured_bass& elt );
		virtual void visitStart( S_forward& elt );
		virtual void visitStart( S_frame_note& elt );
		virtual void visitStart( S_frame& elt );
		virtual void visitStart( S_harmonic& elt );
		virtual void visitStart( S_harmony& elt );
		virtual void visitStart( S_identification& elt );
		virtual void visitStart( S_measure_style& elt );
		virtual void visitStart( S_metronome_note& elt );
		virtual void visitStart( S_metronome_tuplet& elt );
		virtual void visitStart( S_midi_instrument& elt );
		virtual void visitStart( S_notations& elt );
		virtual void visitStart( S_note& elt );
		virtual void visitStart( S_page_layout& elt );
		virtual void visitStart( S_page_margins& elt );
		virtual void visitStart( S_part_group& elt );
		virtual void visitStart( S_pedal_tuning& elt );
		virtual void visitStart( S_pitch& elt );
		virtual void visitStart( S_print& elt );
		virtual void visitStart( S_rest& elt );
		virtual void visitStart( S_root& elt );
		virtual void visitStart( S_scaling& elt );
		virtual void visitStart( S_score_instrument& elt );
		virtual void visitStart( S_score_part& elt );
		virtual void visitStart( S_score_partwise& elt );
		virtual void visitStart( S_slash& elt );
		virtual void visitStart( S_sound& elt );
		virtual void visitStart( S_staff_details& elt );
		virtual void visitStart( S_staff_tuning& elt );
		virtual void visitStart( S_system_layout& elt );
		virtual void visitStart( S_system_margins& elt );
		virtual void visitStart( S_time_modification& elt );
		virtual void visitStart( S_transpose& elt );
		virtual void visitStart( S_tuplet_actual& elt );
		virtual void visitStart( S_tuplet_normal& elt );
		virtual void visitStart( S_tuplet& elt );
		virtual void visitStart( S_unpitched& elt );
		virtual void visitStart( S_work& elt );
};

/*! @} */

}

#endif

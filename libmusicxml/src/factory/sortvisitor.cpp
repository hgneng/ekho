/*
  MusicXML Library
  Copyright (C) Grame 2006-2013

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.

  Grame Research Laboratory, 11, cours de Verdun Gensoul 69002 Lyon - France
  research@grame.fr
*/

#ifdef VC6
# pragma warning (disable : 4786)
#endif

#include <algorithm>
#include <iostream>
#include <map>
#include "sortvisitor.h"
#include "types.h"

using namespace std;

namespace MusicXML2
{

// a set of maps to manage the xml elements order _ one map for each container
static map<int,int> gScorePartwiseOrder;
static map<int,int> gAccordionRegistrationOrder;
static map<int,int> gAccordOrder;
static map<int,int> gAppearanceOrder;
static map<int,int> gAttributesOrder;
static map<int,int> gBackupOrder;
static map<int,int> gBarlineOrder;
static map<int,int> gBassOrder;
static map<int,int> gBeatRepeatOrder;
static map<int,int> gBendOrder;
static map<int,int> gClefOrder;
//static map<int,int> gCreditOrder;			// can't sort the element
static map<int,int> gDefaultsOrder;
static map<int,int> gDegreeOrder;
static map<int,int> gDirectionOrder;
//static map<int,int> gDirectionTypeOrder;	// can't sort the element
static map<int,int> gFiguredBassOrder;
static map<int,int> gFigureOrder;
static map<int,int> gForwardOrder;
static map<int,int> gFrameNoteOrder;
static map<int,int> gFrameOrder;
static map<int,int> gHarmonicOrder;
static map<int,int> gHarmonyOrder;
static map<int,int> gIdentificationOrder;
//static map<int,int> gKeyOrder;			// can't sort the element
//static map<int,int> gLyricOrder;			// can't sort the element
static map<int,int> gMeasureStyleOrder;
static map<int,int> gMetronomeNoteOrder;
//static map<int,int> gMetronomeOrder;		// can't sort the element
static map<int,int> gMetronomeTupletOrder;
static map<int,int> gMidiInstrumentOrder;
static map<int,int> gNotationsOrder;
static map<int,int> gNoteOrder;
//static map<int,int> gOrnamentsOrder;		// can't sort the element
static map<int,int> gPageLayoutOrder;
static map<int,int> gPageMarginsOrder;
static map<int,int> gPartGroupOrder;
static map<int,int> gPedalTuningOrder;
static map<int,int> gPitchOrder;
static map<int,int> gPrintOrder;
static map<int,int> gRestOrder;
static map<int,int> gRootOrder;
static map<int,int> gScalingOrder;
static map<int,int> gScoreInstrumentOrder;
static map<int,int> gScorePartOrder;
static map<int,int> gSlashOrder;
static map<int,int> gSoundOrder;
static map<int,int> gStaffDetailsOrder;
static map<int,int> gStaffTuningOrder;
static map<int,int> gSystemLayoutOrder;
static map<int,int> gSystemMarginsOrder;
static map<int,int> gTimeModificationOrder;
//static map<int,int> gTimeOrder;			// can't sort the element
static map<int,int> gTransposeOrder;
static map<int,int> gTupletActualOrder;
static map<int,int> gTupletNormalOrder;
static map<int,int> gTupletOrder;
static map<int,int> gUnpitchedOrder;
static map<int,int> gWorkOrder;

//________________________________________________________________________
// a comparison class to sort elements
//________________________________________________________________________
class xmlorder {
	map<int,int>&	fOrder;
	Sxmlelement		fContainer;

	public:
				 xmlorder(map<int,int>& order, Sxmlelement container)	: fOrder(order), fContainer(container) {}	
		virtual	~xmlorder() {}
		void	error		(Sxmlelement elt);
		bool	operator()	(Sxmlelement a, Sxmlelement b);
};

void xmlorder::error (Sxmlelement elt)
{
	cerr << "musicxmlfactory warning: Misplaced element " << elt->getName() << " in " << fContainer->getName() << endl;
}

bool xmlorder::operator() (Sxmlelement a, Sxmlelement b)
{
	int aIndex = fOrder[a->getType()];
	int bIndex = fOrder[b->getType()];
	if (aIndex == 0) return false;		// wrong a element: reject to end of list
	if (bIndex == 0) return true;		// wrong b element: reject to end of list
	return aIndex < bIndex;
}

//______________________________________________________________________________
sortvisitor::sortvisitor () 
{
	if (gRootOrder.size() == 0) {

		gScorePartwiseOrder[k_work]				= 1;
		gScorePartwiseOrder[k_movement_number]	= 2;
		gScorePartwiseOrder[k_movement_title]	= 3;
		gScorePartwiseOrder[k_identification]	= 4;
		gScorePartwiseOrder[k_defaults]			= 5;
		gScorePartwiseOrder[k_credit]			= 6;
		gScorePartwiseOrder[k_part_list]		= 7;
		gScorePartwiseOrder[k_part]				= 8;

		gAccordionRegistrationOrder[k_accordion_high]	= 1;
		gAccordionRegistrationOrder[k_accordion_middle] = 2;
		gAccordionRegistrationOrder[k_accordion_low]	= 3;

		gAccordOrder[k_tuning_step]		= 1;
		gAccordOrder[k_tuning_alter]	= 2;
		gAccordOrder[k_tuning_octave]	= 3;

		gAppearanceOrder[k_line_width]		= 1;
		gAppearanceOrder[k_note_size]		= 2;
		gAppearanceOrder[k_other_appearance]= 3;

		gAttributesOrder[k_footnote]		= 1;
		gAttributesOrder[k_level]			= 2;
		gAttributesOrder[k_divisions]		= 3;
		gAttributesOrder[k_key]				= 4;
		gAttributesOrder[k_time]			= 5;
		gAttributesOrder[k_staves]			= 6;
		gAttributesOrder[k_part_symbol]		= 7;
		gAttributesOrder[k_instruments]		= 8;
		gAttributesOrder[k_clef]			= 9;
		gAttributesOrder[k_staff_details]	= 10;
		gAttributesOrder[k_transpose]		= 11;
		gAttributesOrder[k_directive]		= 12;
		gAttributesOrder[k_measure_style]	= 13;

		gBackupOrder[k_duration]	= 1;
		gBackupOrder[k_footnote]	= 2;
		gBackupOrder[k_level]		= 3;

		gBarlineOrder[k_bar_style]	= 1;
		gBarlineOrder[k_footnote]	= 2;
		gBarlineOrder[k_level]		= 3;
		gBarlineOrder[k_wavy_line]	= 4;
		gBarlineOrder[k_segno]		= 5;
		gBarlineOrder[k_coda]		= 6;
		gBarlineOrder[k_fermata]	= 7;
		gBarlineOrder[k_ending]		= 8;
		gBarlineOrder[k_repeat]		= 9;

		gBassOrder[k_bass_step]		= 1;
		gBassOrder[k_bass_alter]	= 2;

		gBeatRepeatOrder[k_slash_type] = 1;
		gBeatRepeatOrder[k_slash_dot]  = 2;

		gBendOrder[k_bend_alter]	= 1;
		gBendOrder[k_pre_bend]		= 2;
		gBendOrder[k_release]		= 2;
		gBendOrder[k_with_bar]		= 3;

		gClefOrder[k_sign]				= 1;
		gClefOrder[k_line]				= 2;
		gClefOrder[k_clef_octave_change]= 3;

		gDefaultsOrder[k_scaling]		= 1;
		gDefaultsOrder[k_page_layout]	= 2;
		gDefaultsOrder[k_system_layout] = 3;
		gDefaultsOrder[k_staff_layout]	= 4;
		gDefaultsOrder[k_appearance]	= 5;
		gDefaultsOrder[k_music_font]	= 6;
		gDefaultsOrder[k_word_font]		= 7;
		gDefaultsOrder[k_lyric_font]	= 8;
		gDefaultsOrder[k_lyric_language]= 9;

		gDegreeOrder[k_degree_value]	= 1;
		gDegreeOrder[k_degree_alter]	= 2;
		gDegreeOrder[k_degree_type]		= 3;

		gDirectionOrder[k_direction_type]	= 1;
		gDirectionOrder[k_offset]			= 2;
		gDirectionOrder[k_footnote]			= 3;
		gDirectionOrder[k_level]			= 4;
		gDirectionOrder[k_voice]			= 5;
		gDirectionOrder[k_staff]			= 6;
		gDirectionOrder[k_sound]			= 7;

		gFiguredBassOrder[k_figure]		= 1;
		gFiguredBassOrder[k_duration]	= 2;
		gFiguredBassOrder[k_footnote]	= 3;
		gFiguredBassOrder[k_level]		= 4;

		gFigureOrder[k_prefix]			= 1;
		gFigureOrder[k_figure_number]	= 2;
		gFigureOrder[k_suffix]			= 3;
		gFigureOrder[k_extend]			= 4;

		gForwardOrder[k_duration]	= 1;
		gForwardOrder[k_footnote]	= 2;
		gForwardOrder[k_level]		= 3;
		gForwardOrder[k_voice]		= 4;
		gForwardOrder[k_staff]		= 5;

		gFrameNoteOrder[k_string]	= 1;
		gFrameNoteOrder[k_fret]		= 2;
		gFrameNoteOrder[k_fingering]= 3;
		gFrameNoteOrder[k_barre]	= 4;

		gFrameOrder[k_frame_strings]= 1;
		gFrameOrder[k_frame_frets]	= 2;
		gFrameOrder[k_first_fret]	= 3;
		gFrameOrder[k_frame_note]	= 4;

		gHarmonicOrder[k_natural]		= 1;
		gHarmonicOrder[k_artificial]	= 1;
		gHarmonicOrder[k_base_pitch]	= 2;
		gHarmonicOrder[k_touching_pitch]= 2;
		gHarmonicOrder[k_sounding_pitch]= 2;

		gHarmonyOrder[k_root]		= 1;
		gHarmonyOrder[k_function]	= 1;
		gHarmonyOrder[k_kind]		= 2;
		gHarmonyOrder[k_inversion]	= 3;
		gHarmonyOrder[k_bass]		= 4;
		gHarmonyOrder[k_degree]		= 5;
		gHarmonyOrder[k_frame]		= 6;
		gHarmonyOrder[k_offset]		= 7;
		gHarmonyOrder[k_footnote]	= 8;
		gHarmonyOrder[k_level]		= 9;
		gHarmonyOrder[k_staff]		= 10;

		gIdentificationOrder[k_creator]			= 1;
		gIdentificationOrder[k_rights]			= 2;
		gIdentificationOrder[k_encoding]		= 3;
		gIdentificationOrder[k_source]			= 4;
		gIdentificationOrder[k_relation]		= 5;
		gIdentificationOrder[k_miscellaneous]	= 6;

		gMeasureStyleOrder[k_multiple_rest] = 1;
		gMeasureStyleOrder[k_measure_repeat]= 2;
		gMeasureStyleOrder[k_beat_repeat]	= 3;
		gMeasureStyleOrder[k_slash]			= 4;

		gMetronomeNoteOrder[k_metronome_type]	= 1;
		gMetronomeNoteOrder[k_metronome_dot]	= 2;
		gMetronomeNoteOrder[k_metronome_beam]	= 3;
		gMetronomeNoteOrder[k_metronome_tuplet] = 4;

		gMetronomeTupletOrder[k_actual_notes]	= 1;
		gMetronomeTupletOrder[k_normal_notes]	= 2;
		gMetronomeTupletOrder[k_normal_type]	= 3;
		gMetronomeTupletOrder[k_normal_dot]		= 4;

		gMidiInstrumentOrder[k_midi_channel]	= 1;
		gMidiInstrumentOrder[k_midi_name]		= 2;
		gMidiInstrumentOrder[k_midi_bank]		= 3;
		gMidiInstrumentOrder[k_midi_program]	= 4;
		gMidiInstrumentOrder[k_midi_unpitched]	= 5;
		gMidiInstrumentOrder[k_volume]			= 6;
		gMidiInstrumentOrder[k_pan]				= 7;
		gMidiInstrumentOrder[k_elevation]		= 8;

		gNotationsOrder[k_footnote] = 1;
		gNotationsOrder[k_level]	= 2;

		gNoteOrder[k_grace]		= 1;
		gNoteOrder[k_cue]		= 1;
		gNoteOrder[k_chord]		= 2;
		gNoteOrder[k_pitch]		= 3;
		gNoteOrder[k_unpitched]	= 3;
		gNoteOrder[k_rest]		= 3;
		gNoteOrder[k_duration]	= 4;
		gNoteOrder[k_tie]		= 5;
		gNoteOrder[k_instrument]= 6;
		gNoteOrder[k_footnote]	= 7;
		gNoteOrder[k_level]		= 8;
		gNoteOrder[k_voice]		= 9;
		gNoteOrder[k_type]		= 10;
		gNoteOrder[k_dot]		= 11;
		gNoteOrder[k_accidental]= 12;
		gNoteOrder[k_time_modification]	= 13;
		gNoteOrder[k_stem]		= 14;
		gNoteOrder[k_notehead]	= 15;
		gNoteOrder[k_staff]		= 16;
		gNoteOrder[k_beam]		= 17;
		gNoteOrder[k_notations]	= 18;
		gNoteOrder[k_lyric]		= 19;

		gPageLayoutOrder[k_page_height] = 1;
		gPageLayoutOrder[k_page_width]	= 2;
		gPageMarginsOrder[k_left_margin]	= 1;
		gPageMarginsOrder[k_right_margin]	= 2;
		gPageMarginsOrder[k_top_margin]		= 3;
		gPageMarginsOrder[k_bottom_margin]	= 4;

		gPartGroupOrder[k_group_name]					= 1;
		gPartGroupOrder[k_group_name_display]			= 2;
		gPartGroupOrder[k_group_abbreviation]			= 3;
		gPartGroupOrder[k_group_abbreviation_display]	= 4;
		gPartGroupOrder[k_group_symbol]					= 5;
		gPartGroupOrder[k_group_barline]				= 6;
		gPartGroupOrder[k_group_time]					= 7;
		gPartGroupOrder[k_footnote]						= 8;
		gPartGroupOrder[k_level]						= 9;

		gPedalTuningOrder[k_pedal_step]		= 1;
		gPedalTuningOrder[k_pedal_alter]	= 2;

		gPitchOrder[k_step]		= 1;
		gPitchOrder[k_alter]	= 2;
		gPitchOrder[k_octave]	= 3;

		gPrintOrder[k_page_layout]				= 1;
		gPrintOrder[k_system_layout]			= 2;
		gPrintOrder[k_staff_layout]				= 3;
		gPrintOrder[k_measure_layout]			= 4;
		gPrintOrder[k_measure_numbering]		= 5;
		gPrintOrder[k_part_name_display]		= 6;
		gPrintOrder[k_part_abbreviation_display]= 7;

		gRestOrder[k_display_step]	= 1;
		gRestOrder[k_display_octave]= 2;

		gRootOrder[k_root_step]		= 1;
		gRootOrder[k_root_alter]	= 2;

		gScalingOrder[k_millimeters]= 1;
		gScalingOrder[k_tenths]		= 2;

		gScoreInstrumentOrder[k_instrument_name]		= 1;
		gScoreInstrumentOrder[k_instrument_abbreviation]= 2;
		gScoreInstrumentOrder[k_solo]					= 3;
		gScoreInstrumentOrder[k_ensemble]				= 3;

		gScorePartOrder[k_identification]			= 1;
		gScorePartOrder[k_part_name]				= 2;
		gScorePartOrder[k_part_name_display]		= 3;
		gScorePartOrder[k_part_abbreviation]		= 4;
		gScorePartOrder[k_part_abbreviation_display]= 5;
		gScorePartOrder[k_group]					= 6;
		gScorePartOrder[k_score_instrument]			= 7;
		gScorePartOrder[k_midi_device]				= 8;
		gScorePartOrder[k_midi_instrument]			= 9;

		gSlashOrder[k_slash_type]	= 1;
		gSlashOrder[k_slash_dot]	= 2;

		gSoundOrder[k_midi_instrument]	= 1;
		gSoundOrder[k_offset]			= 2;

		gStaffDetailsOrder[k_staff_type]	= 1;
		gStaffDetailsOrder[k_staff_lines]	= 2;
		gStaffDetailsOrder[k_staff_tuning]	= 3;
		gStaffDetailsOrder[k_capo]			= 4;
		gStaffDetailsOrder[k_staff_size]	= 5;

		gStaffTuningOrder[k_tuning_step]	= 1;
		gStaffTuningOrder[k_tuning_alter]	= 2;
		gStaffTuningOrder[k_tuning_octave]	= 3;

		gSystemLayoutOrder[k_system_margins]		= 1;
		gSystemLayoutOrder[k_system_distance]		= 2;
		gSystemLayoutOrder[k_top_system_distance]	= 3;

		gSystemMarginsOrder[k_left_margin]	= 1;
		gSystemMarginsOrder[k_right_margin] = 2;

		gTimeModificationOrder[k_actual_notes]	= 1;
		gTimeModificationOrder[k_normal_notes]	= 2;
		gTimeModificationOrder[k_normal_type]	= 3;
		gTimeModificationOrder[k_normal_dot]	= 4;

		gTransposeOrder[k_diatonic]			= 1;
		gTransposeOrder[k_chromatic]		= 2;
		gTransposeOrder[k_octave_change]	= 3;
		gTransposeOrder[k_double]			= 4;

		gTupletActualOrder[k_tuplet_number] = 1;
		gTupletActualOrder[k_tuplet_type]	= 2;
		gTupletActualOrder[k_tuplet_dot]	= 3;

		gTupletNormalOrder[k_tuplet_number] = 1;
		gTupletNormalOrder[k_tuplet_type]	= 2;
		gTupletNormalOrder[k_tuplet_dot]	= 3;

		gTupletOrder[k_tuplet_actual] = 1;
		gTupletOrder[k_tuplet_normal] = 2;

		gUnpitchedOrder[k_display_step]		= 1;
		gUnpitchedOrder[k_display_octave]	= 2;

		gWorkOrder[k_work_number]	= 1;
		gWorkOrder[k_work_title]	= 2;
		gWorkOrder[k_opus]			= 3;
	}
}

//______________________________________________________________________________
void sortvisitor::visitStart( S_accord& elt )
	{ std::sort (elt->elements().begin(), elt->elements().end(), xmlorder(gAccordOrder, elt)); }

void sortvisitor::visitStart( S_accordion_registration& elt )
	{ std::sort (elt->elements().begin(), elt->elements().end(), xmlorder(gAccordionRegistrationOrder, elt)); }

void sortvisitor::visitStart( S_appearance& elt )
	{ std::sort (elt->elements().begin(), elt->elements().end(), xmlorder(gAppearanceOrder, elt)); }

void sortvisitor::visitStart( S_attributes& elt )
	{ std::sort (elt->elements().begin(), elt->elements().end(), xmlorder(gAttributesOrder, elt)); }

void sortvisitor::visitStart( S_backup& elt )
	{ std::sort (elt->elements().begin(), elt->elements().end(), xmlorder(gBackupOrder, elt)); }

void sortvisitor::visitStart( S_barline& elt )
	{ std::sort (elt->elements().begin(), elt->elements().end(), xmlorder(gBarlineOrder, elt)); }

void sortvisitor::visitStart( S_bass& elt )
	{ std::sort (elt->elements().begin(), elt->elements().end(), xmlorder(gBassOrder, elt)); }

void sortvisitor::visitStart( S_beat_repeat& elt )
	{ std::sort (elt->elements().begin(), elt->elements().end(), xmlorder(gBeatRepeatOrder, elt)); }

void sortvisitor::visitStart( S_bend& elt )
	{ std::sort (elt->elements().begin(), elt->elements().end(), xmlorder(gBendOrder, elt)); }

void sortvisitor::visitStart( S_clef& elt )
	{ std::sort (elt->elements().begin(), elt->elements().end(), xmlorder(gClefOrder, elt)); }

void sortvisitor::visitStart( S_defaults& elt )
	{ std::sort (elt->elements().begin(), elt->elements().end(), xmlorder(gDefaultsOrder, elt)); }

void sortvisitor::visitStart( S_degree& elt )
	{ std::sort (elt->elements().begin(), elt->elements().end(), xmlorder(gDegreeOrder, elt)); }

void sortvisitor::visitStart( S_direction& elt )
	{ std::sort (elt->elements().begin(), elt->elements().end(), xmlorder(gDirectionOrder, elt)); }

void sortvisitor::visitStart( S_figure& elt )
	{ std::sort (elt->elements().begin(), elt->elements().end(), xmlorder(gFigureOrder, elt)); }

void sortvisitor::visitStart( S_figured_bass& elt )
	{ std::sort (elt->elements().begin(), elt->elements().end(), xmlorder(gFiguredBassOrder, elt)); }

void sortvisitor::visitStart( S_forward& elt )
	{ std::sort (elt->elements().begin(), elt->elements().end(), xmlorder(gForwardOrder, elt)); }

void sortvisitor::visitStart( S_frame_note& elt )
	{ std::sort (elt->elements().begin(), elt->elements().end(), xmlorder(gFrameNoteOrder, elt)); }

void sortvisitor::visitStart( S_frame& elt )
	{ std::sort (elt->elements().begin(), elt->elements().end(), xmlorder(gFrameOrder, elt)); }

void sortvisitor::visitStart( S_harmonic& elt )
	{ std::sort (elt->elements().begin(), elt->elements().end(), xmlorder(gHarmonicOrder, elt)); }

void sortvisitor::visitStart( S_harmony& elt )
	{ std::sort (elt->elements().begin(), elt->elements().end(), xmlorder(gHarmonyOrder, elt)); }

void sortvisitor::visitStart( S_identification& elt )
	{ std::sort (elt->elements().begin(), elt->elements().end(), xmlorder(gIdentificationOrder, elt)); }

void sortvisitor::visitStart( S_measure_style& elt )
	{ std::sort (elt->elements().begin(), elt->elements().end(), xmlorder(gMeasureStyleOrder, elt)); }

void sortvisitor::visitStart( S_metronome_note& elt )
	{ std::sort (elt->elements().begin(), elt->elements().end(), xmlorder(gMetronomeNoteOrder, elt)); }

void sortvisitor::visitStart( S_metronome_tuplet& elt )
	{ std::sort (elt->elements().begin(), elt->elements().end(), xmlorder(gMetronomeTupletOrder, elt)); }

void sortvisitor::visitStart( S_midi_instrument& elt )
	{ std::sort (elt->elements().begin(), elt->elements().end(), xmlorder(gMidiInstrumentOrder, elt)); }

void sortvisitor::visitStart( S_notations& elt )
	{ std::sort (elt->elements().begin(), elt->elements().end(), xmlorder(gNotationsOrder, elt)); }

void sortvisitor::visitStart( S_note& elt )
	{ std::sort (elt->elements().begin(), elt->elements().end(), xmlorder(gNoteOrder, elt)); }

void sortvisitor::visitStart( S_page_layout& elt )
	{ std::sort (elt->elements().begin(), elt->elements().end(), xmlorder(gPageLayoutOrder, elt)); }

void sortvisitor::visitStart( S_page_margins& elt )
	{ std::sort (elt->elements().begin(), elt->elements().end(), xmlorder(gPageMarginsOrder, elt)); }

void sortvisitor::visitStart( S_part_group& elt )
	{ std::sort (elt->elements().begin(), elt->elements().end(), xmlorder(gPartGroupOrder, elt)); }

void sortvisitor::visitStart( S_pedal_tuning& elt )
	{ std::sort (elt->elements().begin(), elt->elements().end(), xmlorder(gPedalTuningOrder, elt)); }

void sortvisitor::visitStart( S_pitch& elt )
	{ std::sort (elt->elements().begin(), elt->elements().end(), xmlorder(gPitchOrder, elt)); }

void sortvisitor::visitStart( S_print& elt )
	{ std::sort (elt->elements().begin(), elt->elements().end(), xmlorder(gPrintOrder, elt)); }

void sortvisitor::visitStart( S_rest& elt )
	{ std::sort (elt->elements().begin(), elt->elements().end(), xmlorder(gRestOrder, elt)); }

void sortvisitor::visitStart( S_root& elt )
	{ std::sort (elt->elements().begin(), elt->elements().end(), xmlorder(gRootOrder, elt)); }

void sortvisitor::visitStart( S_scaling& elt )
	{ std::sort (elt->elements().begin(), elt->elements().end(), xmlorder(gScalingOrder, elt)); }

void sortvisitor::visitStart( S_score_instrument& elt )
	{ std::sort (elt->elements().begin(), elt->elements().end(), xmlorder(gScoreInstrumentOrder, elt)); }

void sortvisitor::visitStart( S_score_part& elt )
	{ std::sort (elt->elements().begin(), elt->elements().end(), xmlorder(gScorePartOrder, elt)); }

void sortvisitor::visitStart( S_score_partwise& elt )
	{ std::sort (elt->elements().begin(), elt->elements().end(), xmlorder(gScorePartwiseOrder, elt)); }

void sortvisitor::visitStart( S_slash& elt )
	{ std::sort (elt->elements().begin(), elt->elements().end(), xmlorder(gSlashOrder, elt)); }

void sortvisitor::visitStart( S_sound& elt )
	{ std::sort (elt->elements().begin(), elt->elements().end(), xmlorder(gSoundOrder, elt)); }

void sortvisitor::visitStart( S_staff_details& elt )
	{ std::sort (elt->elements().begin(), elt->elements().end(), xmlorder(gStaffDetailsOrder, elt)); }

void sortvisitor::visitStart( S_staff_tuning& elt )
	{ std::sort (elt->elements().begin(), elt->elements().end(), xmlorder(gStaffTuningOrder, elt)); }

void sortvisitor::visitStart( S_system_layout& elt )
	{ std::sort (elt->elements().begin(), elt->elements().end(), xmlorder(gSystemLayoutOrder, elt)); }

void sortvisitor::visitStart( S_system_margins& elt )
	{ std::sort (elt->elements().begin(), elt->elements().end(), xmlorder(gSystemMarginsOrder, elt)); }

void sortvisitor::visitStart( S_time_modification& elt )
	{ std::sort (elt->elements().begin(), elt->elements().end(), xmlorder(gTimeModificationOrder, elt)); }

void sortvisitor::visitStart( S_transpose& elt )
	{ std::sort (elt->elements().begin(), elt->elements().end(), xmlorder(gTransposeOrder, elt)); }

void sortvisitor::visitStart( S_tuplet_actual& elt )
	{ std::sort (elt->elements().begin(), elt->elements().end(), xmlorder(gTupletActualOrder, elt)); }

void sortvisitor::visitStart( S_tuplet_normal& elt )
	{ std::sort (elt->elements().begin(), elt->elements().end(), xmlorder(gTupletNormalOrder, elt)); }

void sortvisitor::visitStart( S_tuplet& elt )
	{ std::sort (elt->elements().begin(), elt->elements().end(), xmlorder(gTupletOrder, elt)); }

void sortvisitor::visitStart( S_unpitched& elt )
	{ std::sort (elt->elements().begin(), elt->elements().end(), xmlorder(gUnpitchedOrder, elt)); }

void sortvisitor::visitStart( S_work& elt )
	{ std::sort (elt->elements().begin(), elt->elements().end(), xmlorder(gWorkOrder, elt)); }



}

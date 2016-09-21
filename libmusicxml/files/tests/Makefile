
files  = $(wildcard *.xml)
gmn := $(patsubst %.xml, %.gmn, $(files))

all : $(gmn)

clean :
	rm -f $(gmn)

%.gmn: %.xml
	xml2guido $< > $@



00a-Basics-Pitches.gmn : 00a-Basics-Pitches.xml
00b-Basics-Intervals.gmn : 00b-Basics-Intervals.xml
00c-Basics-Durations.gmn : 00c-Basics-Durations.xml
00d-Basics-RestDurations.gmn : 00d-Basics-RestDurations.xml
00e-Basics-PitchedRests-PJB.gmn : 00e-Basics-PitchedRests-PJB.xml
00f-Basics-Clefs.gmn : 00f-Basics-Clefs.xml
00g-Basics-Keys.gmn : 00g-Basics-Keys.xml
00h-Basics-TimeSignatures.gmn : 00h-Basics-TimeSignatures.xml
00i-Basics-NoKeyOrClef.gmn : 00i-Basics-NoKeyOrClef.xml
00j-Basics-NoTime-PJB.gmn : 00j-Basics-NoTime-PJB.xml
00k-Basics-Backup.gmn : 00k-Basics-Backup.xml
00l-Basics-MultiMeasureRests.gmn : 00l-Basics-MultiMeasureRests.xml
00l-Basics-Tie-PJB.gmn : 00l-Basics-Tie-PJB.xml
01a-Chord-Rosegarden.gmn : 01a-Chord-Rosegarden.xml
01b-Chords-Rosegarden.gmn : 01b-Chords-Rosegarden.xml
01c-Chords-Rosegarden.gmn : 01c-Chords-Rosegarden.xml
01d-Chords-SchubertStabatMater.gmn : 01d-Chords-SchubertStabatMater.xml
01e-Chords-PickupMeasures.gmn : 01e-Chords-PickupMeasures.xml
02a-Notations-MusicXML.gmn : 02a-Notations-MusicXML.xml
02b-Articulations-Texts-PJB.gmn : 02b-Articulations-Texts-PJB.xml
02c-MultipleNotationChildren-RFK.gmn : 02c-MultipleNotationChildren-RFK.xml
02d-Arpeggio.gmn : 02d-Arpeggio.xml
03a-Directions-MusicXML.gmn : 03a-Directions-MusicXML.xml
03b-AccordionRegistrations-MusicXML.gmn : 03b-AccordionRegistrations-MusicXML.xml
04a-Spanners-Finale.gmn : 04a-Spanners-Finale.xml
04b-Glissando.gmn : 04b-Glissando.xml
04c-Spanners-Noteedit.gmn : 04c-Spanners-Noteedit.xml
04d-Spanners-JScore.gmn : 04d-Spanners-JScore.xml
04e-OctaveShifts-Finale.gmn : 04e-OctaveShifts-Finale.xml
05a-HeaderQuotes-Finale.gmn : 05a-HeaderQuotes-Finale.xml
06a-Lyrics-Finale.gmn : 06a-Lyrics-Finale.xml
06b-MultipleLyrics-Finale.gmn : 06b-MultipleLyrics-Finale.xml
06c-Lyrics-Pianostaff-Finale.gmn : 06c-Lyrics-Pianostaff-Finale.xml
06d-Lyrics-Melisma-Finale.gmn : 06d-Lyrics-Melisma-Finale.xml
06e-Lyrics-Chords-Finale.gmn : 06e-Lyrics-Chords-Finale.xml
06f-Lyrics-GracedNotes-Finale.gmn : 06f-Lyrics-GracedNotes-Finale.xml
06g-Lyrics-NameNumber.gmn : 06g-Lyrics-NameNumber.xml
06h-Lyrics-BeamsMelismata.gmn : 06h-Lyrics-BeamsMelismata.xml
08a-Partorder-Rosegarden.gmn : 08a-Partorder-Rosegarden.xml
08b-StaffGroups-Finale.gmn : 08b-StaffGroups-Finale.xml
08c-More-than-10-parts-Rosegarden.gmn : 08c-More-than-10-parts-Rosegarden.xml
08d-NestedPartsBrackets-Finale.gmn : 08d-NestedPartsBrackets-Finale.xml
08e-LinebrokenInstrumentNames-Finale.gmn : 08e-LinebrokenInstrumentNames-Finale.xml
08f-PianoStaff-PJB.gmn : 08f-PianoStaff-PJB.xml
08g-OverlappingPartGroups-Finale.gmn : 08g-OverlappingPartGroups-Finale.xml
09a-SimpleRepeat-Finale.gmn : 09a-SimpleRepeat-Finale.xml
09b-RepeatWithAlternatives-Finale.gmn : 09b-RepeatWithAlternatives-Finale.xml
09c-Barlines-Finale.gmn : 09c-Barlines-Finale.xml
09d-RepeatMultipleTimes-Finale.gmn : 09d-RepeatMultipleTimes-Finale.xml
09e-Alternatives-Finale.gmn : 09e-Alternatives-Finale.xml
09f-Repeats-Finale.gmn : 09f-Repeats-Finale.xml
09g-Endings-Finale.gmn : 09g-Endings-Finale.xml
09h-RepeatsNoEndBar-Finale.gmn : 09h-RepeatsNoEndBar-Finale.xml
09i-MidmeasureBarline.gmn : 09i-MidmeasureBarline.xml
09j-Midmeasure-Clef-Finale.gmn : 09j-Midmeasure-Clef-Finale.xml
09k-Upbeats-ImplicitMeasures-Finale.gmn : 09k-Upbeats-ImplicitMeasures-Finale.xml
09l-PickupMeasure-SecondVoiceMessup.gmn : 09l-PickupMeasure-SecondVoiceMessup.xml
10a-TwoVoicesOnStaff-Finale.gmn : 10a-TwoVoicesOnStaff-Finale.xml
12a-TripletsDuration-NoBracket-PJB.gmn : 12a-TripletsDuration-NoBracket-PJB.xml
12b-Tuplets-Finale.gmn : 12b-Tuplets-Finale.xml
13a-GraceNotes-Finale.gmn : 13a-GraceNotes-Finale.xml
13b-ChordAsGraceNote-Finale.gmn : 13b-ChordAsGraceNote-Finale.xml
14a-MultistaffClefDynamics-Finale.gmn : 14a-MultistaffClefDynamics-Finale.xml
14b-DifferentKeys-PJB.gmn : 14b-DifferentKeys-PJB.xml
14b-DifferentKeysAfterBackup-PJB.gmn : 14b-DifferentKeysAfterBackup-PJB.xml
14c-StaffChange-Finale.gmn : 14c-StaffChange-Finale.xml
15a-Percussion-Finale.gmn : 15a-Percussion-Finale.xml
17a-Chords-Finale.gmn : 17a-Chords-Finale.xml
17b-Fretboards-Finale.gmn : 17b-Fretboards-Finale.xml
17c-ChordsFrets-Finale.gmn : 17c-ChordsFrets-Finale.xml
17d-ChordsFretsOnMultistaff-Finale.gmn : 17d-ChordsFretsOnMultistaff-Finale.xml
17e-TabStaves-Finale.gmn : 17e-TabStaves-Finale.xml
19a-PageLayout-PrintMusic.gmn : 19a-PageLayout-PrintMusic.xml
99a-Sibelius5-IgnoreBeaming.gmn : 99a-Sibelius5-IgnoreBeaming.xml
99b-Lyrics-BeamsMelismata-IgnoreBeams.gmn : 99b-Lyrics-BeamsMelismata-IgnoreBeams.xml

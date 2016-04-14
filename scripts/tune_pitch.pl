# tune pitch with praat
`for N in *1*.wav; do echo "Read from file... $N"; echo "Change gender... 75.0 600.0 1.0 330 1.0 1.0"; echo "nowarn Write to WAV file... $N"; echo "Remove"; done > temp.praat ; praat temp.praat`

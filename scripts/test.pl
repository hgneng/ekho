use eGuideDog::Wav;

$w=eGuideDog::Wav->new("/home/hgneng/e-guidedog/eGuideDog_TTS/out.wav");
$w->load();
$w->play();
print $w->get_volume, "\n";
$w->change_volume(1.5);
$w->play();
print $w->get_volume, "\n";
$w->save('out2.wav');

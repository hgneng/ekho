# ekho
Chinese text-to-speech engine. It's part of <a href='http://www.eguidedog.net'>eGuideDog project</a>. Here is <a href='http://www.eguidedog.net/ekho.php'>Ekho TTS's home page</a>.

This repository is forked from <a href='https://sourceforge.net/p/e-guidedog/code/HEAD/tree/eGuideDog_TTS/'>SourceForge</a> at version r2418 (and patch to r2478).

Voice files are not included. We can get them from a distribution package or <a href='https://sourceforge.net/projects/e-guidedog/files/Ekho-Voice-Data/0.2/'>Ekho Voice Data's download page</a>. To replace Mandarin voice files, we can replace the whole pinyin folder, then remove pinyin.index and pinyin.voice. Re-running ekho will generate new pinyin.index and pinyin.voice from new voice files.

In order to create your own voice, please refere to <a href='http://eguidedog.net/doc/doc_make_new_voice_cn.php'>如何为Ekho添加新的声音</a>.
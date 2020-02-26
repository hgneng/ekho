/*
 * kali/Kali/kali.h - Shim for Kali
 * to be able to build the Kali module without the Kali SDK.
 *
 * Copyright (C) 2018 Hypra
 *
 * This is free software; you can redistribute it and/or modify it
 * under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2, or (at your option)
 * any later version.
 *
 * This software is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <https://www.gnu.org/licenses/>.
 */

typedef struct {
	int bits;
	int num_channels;
	int sample_rate;
	int num_samples;
	const signed short *samples;
} AudioTrackKali;

extern "C" {

extern bool initGlobal(void);
extern bool initParle(void);
extern bool initTrans(void);
extern bool initAnalyse(void);
extern bool initKali(void);

extern const AudioTrackKali *GetBufMultiKaliStd(short nK);
extern void SetSortieBufMultiKaliStd(short nK, bool sortieBuf);
extern void SetSortieSonMultiKaliStd(short nK, bool sortieSon);

extern short GetDebitDefautKaliStd(void);
extern short GetDebitMinKaliStd(void);
extern short GetDebitMaxKaliStd(void);
extern void SetDebitKali(short debit);

extern short GetVolumeDefautKaliStd(void);
extern short GetVolumeMinKaliStd(void);
extern short GetVolumeMaxKaliStd(void);
extern void SetVolumeKali(short volume);

extern short GetHauteurDefautKaliStd(void);
extern short GetHauteurMinKaliStd(void);
extern short GetHauteurMaxKaliStd(void);
extern void SetHauteurKali(short hauteur);

extern void SetModeLectureKali(short modeLecture);

extern short GetNLangueVoixKaliStd(short nVoix);
extern char *GetNomLangueKali(short nLangue, char *nomLangue);
extern void SetLangueKali(short nLangue);
extern short GetNbVoixKali(void);
extern char *GetNomVoixKali(short nVoix, char *nomVoix);
extern void SetVoixKali(short nVoix);

extern short MessageKali(unsigned char* texte);
extern short QueryIndexKali(void);

extern bool quitteGlobal(void);
extern bool quitteParle(void);
extern bool quitteTrans(void);
extern bool quitteAnalyse(void);

}

/*

This file is part of FFTS -- The Fastest Fourier Transform in the South

Copyright (c) 2015-2016, Jukka Ojanen <jukka.ojanen@kolumbus.fi>

All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:
* Redistributions of source code must retain the above copyright
notice, this list of conditions and the following disclaimer.
* Redistributions in binary form must reproduce the above copyright
notice, this list of conditions and the following disclaimer in the
documentation and/or other materials provided with the distribution.
* Neither the name of the organization nor the
names of its contributors may be used to endorse or promote products
derived from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL ANTHONY M. BLAKE BE LIABLE FOR ANY
DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
(INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

*/

#include "ffts_trig.h"
#include "ffts_dd.h"

/*
*  For more information on algorithms:
*
*  D. Potts, G. Steidl, M. Tasche, Numerical stability of fast
*  trigonometric transforms — a worst case study,
*  J. Concrete Appl. Math. 1 (2003) 1–36
*
*  O. Buneman, Stable on–line creation of sines and cosines of
*  successive angles, Proc. IEEE 75, 1434 – 1435 (1987).
*/

/* An union to initialize doubles using byte presentation,
*  and to avoid breaking strict-aliasing rules
*/

/* TODO: we need macros to take care endianess */
typedef union ffts_double {
    int32_t i[2];
    double  d;
} ffts_double_t;

/* 1/(2*cos(pow(2,-p)*pi)) */
static const FFTS_ALIGN(16) ffts_double_t half_secant[66] = {
    { { 0x00000000, 0x3fe00000 } }, { { 0xc9be45de, 0x3be3bd3c } },
    { { 0x00000000, 0x3fe00000 } }, { { 0xc9be45de, 0x3c03bd3c } },
    { { 0x00000000, 0x3fe00000 } }, { { 0xc9be45de, 0x3c23bd3c } },
    { { 0x00000000, 0x3fe00000 } }, { { 0xc9be45de, 0x3c43bd3c } },
    { { 0x00000000, 0x3fe00000 } }, { { 0xc9be45de, 0x3c63bd3c } },
    { { 0x00000000, 0x3fe00000 } }, { { 0xc9be45df, 0x3c83bd3c } },
    { { 0x00000001, 0x3fe00000 } }, { { 0x4df22efd, 0x3c7de9e6 } },
    { { 0x00000005, 0x3fe00000 } }, { { 0x906e8725, 0xbc60b0cd } },
    { { 0x00000014, 0x3fe00000 } }, { { 0x906e8357, 0xbc80b0cd } },
    { { 0x0000004f, 0x3fe00000 } }, { { 0x0dce83c9, 0xbc5619b2 } },
    { { 0x0000013c, 0x3fe00000 } }, { { 0x0dc6e79a, 0xbc7619b2 } },
    { { 0x000004ef, 0x3fe00000 } }, { { 0xe4af1240, 0x3c83cc9b } },
    { { 0x000013bd, 0x3fe00000 } }, { { 0x2d14c08a, 0x3c7e64df } },
    { { 0x00004ef5, 0x3fe00000 } }, { { 0x47a85465, 0xbc59b20b } },
    { { 0x00013bd4, 0x3fe00000 } }, { { 0xab79c897, 0xbc79b203 } },
    { { 0x0004ef4f, 0x3fe00000 } }, { { 0x15019a96, 0x3c79386b } },
    { { 0x0013bd3d, 0x3fe00000 } }, { { 0x7d6dbf4b, 0xbc7b16b7 } },
    { { 0x004ef4f3, 0x3fe00000 } }, { { 0xf30832e0, 0x3c741ee4 } },
    { { 0x013bd3cd, 0x3fe00000 } }, { { 0xd3bcd4bb, 0xbc83f41e } },
    { { 0x04ef4f34, 0x3fe00000 } }, { { 0xdd75aebb, 0xbc82ef06 } },
    { { 0x13bd3cde, 0x3fe00000 } }, { { 0xb2b41b3d, 0x3c52d979 } },
    { { 0x4ef4f46c, 0x3fe00000 } }, { { 0x4f0fb458, 0xbc851db3 } },
    { { 0x3bd3e0e7, 0x3fe00001 } }, { { 0x8a0ce3f0, 0x3c58dbab } },
    { { 0xef507722, 0x3fe00004 } }, { { 0x2a8ec295, 0x3c83e351 } },
    { { 0xbd5114f9, 0x3fe00013 } }, { { 0xc4c0d92d, 0x3c8b3ca4 } },
    { { 0xf637de7d, 0x3fe0004e } }, { { 0xb74de729, 0x3c45974e } },
    { { 0xe8190891, 0x3fe0013b } }, { { 0x26edf4da, 0xbc814c20 } },
    { { 0x9436640e, 0x3fe004f0 } }, { { 0xe2b34b50, 0x3c8091ab } },
    { { 0x9c61d971, 0x3fe013d1 } }, { { 0x6ce01b8e, 0x3c7f7df7 } },
    { { 0xd17cba53, 0x3fe0503e } }, { { 0x74ad7633, 0xbc697609 } },
    { { 0x7bdb3895, 0x3fe1517a } }, { { 0x82f9091b, 0xbc8008d1 } },
    { { 0x00000000, 0x00000000 } }, { { 0x00000000, 0x00000000 } },
    { { 0x00000000, 0x00000000 } }, { { 0x00000000, 0x00000000 } }
};

/* cos(pow(2,-p)*pi), sin(pow(2,-p)*pi) */
static const FFTS_ALIGN(32) ffts_double_t cos_sin_pi_table[132] = {
    { { 0x00000000, 0x3ff00000 } }, { { 0x54442d18, 0x3df921fb } },
    { { 0xc9be45de, 0xbbf3bd3c } }, { { 0xbb77974f, 0x3a91a390 } },
    { { 0x00000000, 0x3ff00000 } }, { { 0x54442d18, 0x3e0921fb } },
    { { 0xc9be45de, 0xbc13bd3c } }, { { 0x54a14928, 0x3aa19bd0 } },
    { { 0x00000000, 0x3ff00000 } }, { { 0x54442d18, 0x3e1921fb } },
    { { 0xc9be45de, 0xbc33bd3c } }, { { 0xb948108a, 0x3ab17cce } },
    { { 0x00000000, 0x3ff00000 } }, { { 0x54442d18, 0x3e2921fb } },
    { { 0xc9be45de, 0xbc53bd3c } }, { { 0x4be32e14, 0x3ac100c8 } },
    { { 0x00000000, 0x3ff00000 } }, { { 0x54442d18, 0x3e3921fb } },
    { { 0xc9be45de, 0xbc73bd3c } }, { { 0x2c9f4879, 0x3ace215d } },
    { { 0xffffffff, 0x3fefffff } }, { { 0x54442d18, 0x3e4921fb } },
    { { 0x6c837443, 0x3c888586 } }, { { 0x0005f376, 0x3acd411f } },
    { { 0xfffffffe, 0x3fefffff } }, { { 0x54442d18, 0x3e5921fb } },
    { { 0x4df22ef1, 0xbc8de9e6 } }, { { 0x9937209e, 0xbaf7b153 } },
    { { 0xfffffff6, 0x3fefffff } }, { { 0x54442d16, 0x3e6921fb } },
    { { 0x906e88aa, 0x3c70b0cd } }, { { 0xfe19968a, 0xbb03b7c0 } },
    { { 0xffffffd9, 0x3fefffff } }, { { 0x54442d0e, 0x3e7921fb } },
    { { 0xdf22ed26, 0xbc8e9e64 } }, { { 0x8d1b6ffb, 0xbaee8bb4 } },
    { { 0xffffff62, 0x3fefffff } }, { { 0x54442cef, 0x3e8921fb } },
    { { 0x0dd18f0f, 0x3c6619b2 } }, { { 0x7f2b20fb, 0xbb00e133 } },
    { { 0xfffffd88, 0x3fefffff } }, { { 0x54442c73, 0x3e9921fb } },
    { { 0x0dd314b2, 0x3c8619b2 } }, { { 0x619fdf6e, 0xbb174e98 } },
    { { 0xfffff621, 0x3fefffff } }, { { 0x54442a83, 0x3ea921fb } },
    { { 0x3764acf5, 0x3c8866c8 } }, { { 0xf5b2407f, 0xbb388215 } },
    { { 0xffffd886, 0x3fefffff } }, { { 0x544422c2, 0x3eb921fb } },
    { { 0x20e7a944, 0xbc8e64df } }, { { 0x7b9b9f23, 0x3b5a0961 } },
    { { 0xffff6216, 0x3fefffff } }, { { 0x544403c1, 0x3ec921fb } },
    { { 0x52ee25ea, 0x3c69b20e } }, { { 0x4df6a86a, 0xbb5999d9 } },
    { { 0xfffd8858, 0x3fefffff } }, { { 0x544387ba, 0x3ed921fb } },
    { { 0xd8910ead, 0x3c89b20f } }, { { 0x0809d04d, 0x3b77d9db } },
    { { 0xfff62162, 0x3fefffff } }, { { 0x544197a1, 0x3ee921fb } },
    { { 0x438d3925, 0xbc8937a8 } }, { { 0xa5d27f7a, 0xbb858b02 } },
    { { 0xffd88586, 0x3fefffff } }, { { 0x5439d73a, 0x3ef921fb } },
    { { 0x94b3ddd2, 0x3c8b22e4 } }, { { 0xf8a3b73d, 0xbb863c7f } },
    { { 0xff62161a, 0x3fefffff } }, { { 0x541ad59e, 0x3f0921fb } },
    { { 0x7ea469b2, 0xbc835c13 } }, { { 0xb8cee262, 0x3bae9860 } },
    { { 0xfd885867, 0x3fefffff } }, { { 0x539ecf31, 0x3f1921fb } },
    { { 0x23a32e63, 0xbc77d556 } }, { { 0xfcd23a30, 0x3b96b111 } },
    { { 0xf621619c, 0x3fefffff } }, { { 0x51aeb57c, 0x3f2921fb } },
    { { 0xbbbd8fe6, 0xbc87507d } }, { { 0x4916c435, 0xbbca6e1d } },
    { { 0xd8858675, 0x3fefffff } }, { { 0x49ee4ea6, 0x3f3921fb } },
    { { 0x54748eab, 0xbc879f0e } }, { { 0x744a453e, 0x3bde894d } },
    { { 0x62161a34, 0x3fefffff } }, { { 0x2aecb360, 0x3f4921fb } },
    { { 0xb1f9b9c4, 0xbc6136dc } }, { { 0x7e566b4c, 0x3be87615 } },
    { { 0x88586ee6, 0x3feffffd } }, { { 0xaee6472e, 0x3f5921fa } },
    { { 0xf173ae5b, 0x3c81af64 } }, { { 0x284a9df8, 0xbbfee52e } },
    { { 0x21621d02, 0x3feffff6 } }, { { 0xbecca4ba, 0x3f6921f8 } },
    { { 0xebc82813, 0xbc76acfc } }, { { 0x7bcab5b2, 0x3c02ba40 } },
    { { 0x858e8a92, 0x3fefffd8 } }, { { 0xfe670071, 0x3f7921f0 } },
    { { 0x1883bcf7, 0x3c8359c7 } }, { { 0xfe6b7a9b, 0x3bfab967 } },
    { { 0x169b92db, 0x3fefff62 } }, { { 0xfcdec784, 0x3f8921d1 } },
    { { 0xc81fbd0d, 0x3c85dda3 } }, { { 0xbe836d9d, 0x3c29878e } },
    { { 0x6084cd0d, 0x3feffd88 } }, { { 0xf7a3667e, 0x3f992155 } },
    { { 0x4556e4cb, 0xbc81354d } }, { { 0x091a0130, 0xbbfb1d63 } },
    { { 0xe3796d7e, 0x3feff621 } }, { { 0xf10dd814, 0x3fa91f65 } },
    { { 0x2e24aa15, 0xbc6c57bc } }, { { 0x0d569a90, 0xbc2912bd } },
    { { 0xa3d12526, 0x3fefd88d } }, { { 0xbc29b42c, 0x3fb917a6 } },
    { { 0x378811c7, 0xbc887df6 } }, { { 0xd26ed688, 0xbc3e2718 } },
    { { 0xcff75cb0, 0x3fef6297 } }, { { 0x3c69a60b, 0x3fc8f8b8 } },
    { { 0x2a361fd3, 0x3c756217 } }, { { 0xb9ff8d82, 0xbc626d19 } },
    { { 0xcf328d46, 0x3fed906b } }, { { 0xa6aea963, 0x3fd87de2 } },
    { { 0x10231ac2, 0x3c7457e6 } }, { { 0xd3d5a610, 0xbc672ced } },
    { { 0x667f3bcd, 0x3fe6a09e } }, { { 0x667f3bcd, 0x3fe6a09e } },
    { { 0x13b26456, 0xbc8bdd34 } }, { { 0x13b26456, 0xbc8bdd34 } },
    { { 0x00000000, 0x00000000 } }, { { 0x00000000, 0x3ff00000 } },
    { { 0x00000000, 0x00000000 } }, { { 0x00000000, 0x00000000 } }
};

/* cos(pi*k/256), sin(pi*k/256) */
static const FFTS_ALIGN(32) ffts_double_t cos_sin_table[260] = {
    { { 0x00000000, 0x3FF00000 } }, { { 0x00000000, 0x00000000 } },
    { { 0x00000000, 0x00000000 } }, { { 0x00000000, 0x00000000 } },
    { { 0x169B92DB, 0x3FEFFF62 } }, { { 0xC81FBD0D, 0x3C85DDA3 } },
    { { 0xFCDEC784, 0x3F8921D1 } }, { { 0xBE836D9D, 0x3C29878E } },
    { { 0x6084CD0D, 0x3FEFFD88 } }, { { 0x4556E4CB, 0xBC81354D } },
    { { 0xF7A3667E, 0x3F992155 } }, { { 0x091A0130, 0xBBFB1D63 } },
    { { 0xEFFEF75D, 0x3FEFFA72 } }, { { 0xCDB25956, 0xBC88B4CD } },
    { { 0x759455CD, 0x3FA2D865 } }, { { 0x5BA93AC0, 0x3C2686F6 } },
    { { 0xE3796D7E, 0x3FEFF621 } }, { { 0x2E24AA15, 0xBC6C57BC } },
    { { 0xF10DD814, 0x3FA91F65 } }, { { 0x0D569A90, 0xBC2912BD } },
    { { 0x658E71AD, 0x3FEFF095 } }, { { 0xE18A4B9E, 0x3C801A8C } },
    { { 0x79F820E0, 0x3FAF656E } }, { { 0xE392BFFE, 0xBC22E1EB } },
    { { 0xAD01883A, 0x3FEFE9CD } }, { { 0xD0C67E35, 0x3C6521EC } },
    { { 0x92CE19F6, 0x3FB2D520 } }, { { 0xA8BF6B2C, 0xBC49A088 } },
    { { 0xFCBD5B09, 0x3FEFE1CA } }, { { 0x202A884E, 0x3C6A23E3 } },
    { { 0x0A9AA419, 0x3FB5F6D0 } }, { { 0xD03F6C9A, 0xBC4F4022 } },
    { { 0xA3D12526, 0x3FEFD88D } }, { { 0x378811C7, 0xBC887DF6 } },
    { { 0xBC29B42C, 0x3FB917A6 } }, { { 0xD26ED688, 0xBC3E2718 } },
    { { 0xFD6DA67B, 0x3FEFCE15 } }, { { 0x830D4C09, 0xBC75DD6F } },
    { { 0xC79EC2D5, 0x3FBC3785 } }, { { 0xF133FB21, 0xBC24F39D } },
    { { 0x70E19FD3, 0x3FEFC264 } }, { { 0x68ECACEE, 0x3C81EC86 } },
    { { 0x56A9730E, 0x3FBF564E } }, { { 0x729AE56D, 0x3C4A2704 } },
    { { 0x7195D741, 0x3FEFB579 } }, { { 0x7397CC08, 0x3C71BFAC } },
    { { 0xCEDAF577, 0x3FC139F0 } }, { { 0x4D1B3CFA, 0xBC652343 } },
    { { 0x7F08A517, 0x3FEFA755 } }, { { 0xCA13571F, 0xBC87A0A8 } },
    { { 0x6E8E613A, 0x3FC2C810 } }, { { 0xA89A11E0, 0x3C513000 } },
    { { 0x24C9099B, 0x3FEF97F9 } }, { { 0xEEA5963B, 0xBC8E2AE0 } },
    { { 0xB1293E5A, 0x3FC45576 } }, { { 0x4119F7B1, 0xBC5285A2 } },
    { { 0xFA714BA9, 0x3FEF8764 } }, { { 0x778FFCB6, 0x3C7AB256 } },
    { { 0x448B3FC6, 0x3FC5E214 } }, { { 0x779DDAC6, 0x3C6531FF } },
    { { 0xA3A12077, 0x3FEF7599 } }, { { 0xD743195C, 0x3C884F31 } },
    { { 0xDE50BF31, 0x3FC76DD9 } }, { { 0xEC501B2F, 0x3C61D5EE } },
    { { 0xCFF75CB0, 0x3FEF6297 } }, { { 0x2A361FD3, 0x3C756217 } },
    { { 0x3C69A60B, 0x3FC8F8B8 } }, { { 0xB9FF8D82, 0xBC626D19 } },
    { { 0x3B0B2F2D, 0x3FEF4E60 } }, { { 0xE695AC05, 0xBC78EE01 } },
    { { 0x25B00451, 0x3FCA82A0 } }, { { 0xFFD084AD, 0xBC687905 } },
    { { 0xAC64E589, 0x3FEF38F3 } }, { { 0xB51F72E6, 0xBC7D7BAF } },
    { { 0x6A7E4F63, 0x3FCC0B82 } }, { { 0x9E521935, 0xBC1AF143 } },
    { { 0xF7763ADA, 0x3FEF2252 } }, { { 0x1C8D94AB, 0xBC820CB8 } },
    { { 0xE5454311, 0x3FCD934F } }, { { 0x277107AD, 0x3C675B92 } },
    { { 0xFB9230D7, 0x3FEF0A7E } }, { { 0xDC6B4989, 0x3C752C7A } },
    { { 0x7B215F1B, 0x3FCF19F9 } }, { { 0xF11DA2C4, 0xBC642DEE } },
    { { 0xA3E473C2, 0x3FEEF178 } }, { { 0x67FE774F, 0x3C86310A } },
    { { 0x0E37FDAE, 0x3FD04FB8 } }, { { 0xB72583CC, 0xBC0412CD } },
    { { 0xE7684963, 0x3FEED740 } }, { { 0x91F59CC2, 0x3C7E82C7 } },
    { { 0x62B1F677, 0x3FD111D2 } }, { { 0x0AB7AA9A, 0x3C7824C2 } },
    { { 0xC8DF0B74, 0x3FEEBBD8 } }, { { 0x615E7277, 0x3C7C6C8C } },
    { { 0x3F4CDB3E, 0x3FD1D344 } }, { { 0x1C13519E, 0xBC6720D4 } },
    { { 0x56C62DDA, 0x3FEE9F41 } }, { { 0xE2E3F81E, 0x3C8760B1 } },
    { { 0x2ED59F06, 0x3FD29406 } }, { { 0xA2C4612D, 0xBC75D28D } },
    { { 0xAB4CD10D, 0x3FEE817B } }, { { 0x686B5E0A, 0xBC7D0AFE } },
    { { 0xC2E18152, 0x3FD35410 } }, { { 0x2F96E062, 0xBC73CB00 } },
    { { 0xEC48E112, 0x3FEE6288 } }, { { 0xF2847754, 0xBC616B56 } },
    { { 0x94176601, 0x3FD4135C } }, { { 0x4AFA2518, 0x3C70C97C } },
    { { 0x4B2BC17E, 0x3FEE426A } }, { { 0x89744882, 0x3C8A8738 } },
    { { 0x4278E76A, 0x3FD4D1E2 } }, { { 0x18792858, 0x3C624172 } },
    { { 0x04F686E5, 0x3FEE2121 } }, { { 0x6C126527, 0xBC8014C7 } },
    { { 0x75AB1FDD, 0x3FD58F9A } }, { { 0xD58CF620, 0xBC1EFDC0 } },
    { { 0x622DBE2B, 0x3FEDFEAE } }, { { 0x88425567, 0xBC8514EA } },
    { { 0xDD3F27C6, 0x3FD64C7D } }, { { 0x4A664121, 0x3C510D2B } },
    { { 0xB6CCC23C, 0x3FEDDB13 } }, { { 0xC6107DB3, 0x3C883C37 } },
    { { 0x30FA459F, 0x3FD70885 } }, { { 0xE0864C5D, 0xBC744B19 } },
    { { 0x6238A09B, 0x3FEDB652 } }, { { 0xEAE69460, 0xBC7ADEE7 } },
    { { 0x311DCCE7, 0x3FD7C3A9 } }, { { 0x1EF3E8D9, 0x3C19A3F2 } },
    { { 0xCF328D46, 0x3FED906B } }, { { 0x10231AC2, 0x3C7457E6 } },
    { { 0xA6AEA963, 0x3FD87DE2 } }, { { 0xD3D5A610, 0xBC672CED } },
    { { 0x73C9E68B, 0x3FED6961 } }, { { 0xC6393D55, 0xBC7E8C61 } },
    { { 0x63BC93D7, 0x3FD9372A } }, { { 0x9E5AD5B1, 0x3C668431 } },
    { { 0xD14DC93A, 0x3FED4134 } }, { { 0x95D25AF2, 0xBC84EF52 } },
    { { 0x43A8ED8A, 0x3FD9EF79 } }, { { 0x290BDBAB, 0x3C66DA81 } },
    { { 0x743E35DC, 0x3FED17E7 } }, { { 0x3540130A, 0xBC5101DA } },
    { { 0x2B6D3FCA, 0x3FDAA6C8 } }, { { 0x6EE5CCF7, 0xBC7D5F10 } },
    { { 0xF43CC773, 0x3FECED7A } }, { { 0xB5AB58AE, 0xBC5E7B6B } },
    { { 0x09E15CC0, 0x3FDB5D10 } }, { { 0xCB974183, 0x3C65B362 } },
    { { 0xF3FCFC5C, 0x3FECC1F0 } }, { { 0x3B68F6AB, 0x3C7E5761 } },
    { { 0xD8011EE7, 0x3FDC1249 } }, { { 0xBB515206, 0xBC7813AA } },
    { { 0x213411F5, 0x3FEC954B } }, { { 0x1E946603, 0xBC52FB76 } },
    { { 0x9931C45E, 0x3FDCC66E } }, { { 0x59C37F8F, 0x3C56850E } },
    { { 0x3488739B, 0x3FEC678B } }, { { 0xC7C5FF5B, 0x3C6D86CA } },
    { { 0x5B86E389, 0x3FDD7977 } }, { { 0x87BC0575, 0x3C7550EC } },
    { { 0xF180BDB1, 0x3FEC38B2 } }, { { 0x757C8D07, 0xBC76E0B1 } },
    { { 0x3806F63B, 0x3FDE2B5D } }, { { 0x1D3C6841, 0x3C5E0D89 } },
    { { 0x26725549, 0x3FEC08C4 } }, { { 0xD80E2946, 0x3C5B157F } },
    { { 0x52EF78D6, 0x3FDEDC19 } }, { { 0xC33EDEE6, 0xBC7DD0F7 } },
    { { 0xAC6F952A, 0x3FEBD7C0 } }, { { 0x32AC700A, 0xBC8825A7 } },
    { { 0xDBF89ABA, 0x3FDF8BA4 } }, { { 0xC1B776B8, 0xBC32EC1F } },
    { { 0x673590D2, 0x3FEBA5AA } }, { { 0x370753B6, 0x3C87EA4E } },
    { { 0x874C3EB7, 0x3FE01CFC } }, { { 0xE7C2368C, 0xBC734A35 } },
    { { 0x45196E3E, 0x3FEB7283 } }, { { 0x324E6D61, 0xBC8BC69F } },
    { { 0x9922FFEE, 0x3FE07387 } }, { { 0x4347406C, 0xBC8A5A01 } },
    { { 0x3EF55712, 0x3FEB3E4D } }, { { 0xBF11A493, 0xBC8EB6B8 } },
    { { 0x4D5D898F, 0x3FE0C970 } }, { { 0xDE6EE9B2, 0xBC88D3D7 } },
    { { 0x58150200, 0x3FEB090A } }, { { 0x300FFCCE, 0xBC8926DA } },
    { { 0x541B4B23, 0x3FE11EB3 } }, { { 0x69ABE4F1, 0xBC8EF23B } },
    { { 0x9E21D511, 0x3FEAD2BC } }, { { 0x07BEA548, 0xBC847FBE } },
    { { 0x63DEDB49, 0x3FE1734D } }, { { 0xCCC50575, 0xBC87EEF2 } },
    { { 0x290EA1A3, 0x3FEA9B66 } }, { { 0xE8B6DAC8, 0x3C39F630 } },
    { { 0x39AE68C8, 0x3FE1C73B } }, { { 0x267F6600, 0x3C8B25DD } },
    { { 0x1B02FAE2, 0x3FEA6309 } }, { { 0x52248D10, 0xBC7E9111 } },
    { { 0x9933EB59, 0x3FE21A79 } }, { { 0x77C68FB2, 0xBC83A7B1 } },
    { { 0xA0462782, 0x3FEA29A7 } }, { { 0x015DF175, 0xBC7128BB } },
    { { 0x4CDD12DF, 0x3FE26D05 } }, { { 0x3EF3770C, 0xBC85DA74 } },
    { { 0xEF29AF94, 0x3FE9EF43 } }, { { 0xB60445C2, 0x3C7B1DFC } },
    { { 0x25FAF3EA, 0x3FE2BEDB } }, { { 0xC796EE46, 0xBC514981 } },
    { { 0x47F38741, 0x3FE9B3E0 } }, { { 0x86712474, 0xBC830EE2 } },
    { { 0xFCE17035, 0x3FE30FF7 } }, { { 0x26F74A6F, 0xBC6EFCC6 } },
    { { 0xF4C7D742, 0x3FE9777E } }, { { 0xA240665E, 0xBC815479 } },
    { { 0xB10659F3, 0x3FE36058 } }, { { 0xA35857E7, 0xBC81FCB3 } },
    { { 0x499263FB, 0x3FE93A22 } }, { { 0xA920DF0B, 0x3C83D419 } },
    { { 0x292050B9, 0x3FE3AFFA } }, { { 0xE3954964, 0x3C7E3E25 } },
    { { 0xA3EF940D, 0x3FE8FBCC } }, { { 0x9C86F2F1, 0xBC66DFA9 } },
    { { 0x534556D4, 0x3FE3FED9 } }, { { 0x608C5061, 0x3C836916 } },
    { { 0x6B151741, 0x3FE8BC80 } }, { { 0x2ED1336D, 0xBC82C5E1 } },
    { { 0x25091DD6, 0x3FE44CF3 } }, { { 0x2CFDC6B3, 0x3C68076A } },
    { { 0x0FBA2EBF, 0x3FE87C40 } }, { { 0x0C3F64CD, 0xBC82DABC } },
    { { 0x9B9B0939, 0x3FE49A44 } }, { { 0x6D719B94, 0xBC827EE1 } },
    { { 0x0BFF976E, 0x3FE83B0E } }, { { 0xF8EA3475, 0xBC76F420 } },
    { { 0xBBE3E5E9, 0x3FE4E6CA } }, { { 0xEDCEB327, 0x3C63C293 } },
    { { 0xE3571771, 0x3FE7F8EC } }, { { 0xCE93C917, 0xBC89C8D8 } },
    { { 0x92A35596, 0x3FE53282 } }, { { 0x89DA0257, 0xBC7A12EB } },
    { { 0x226AAFAF, 0x3FE7B5DF } }, { { 0xACDF0AD7, 0xBC70F537 } },
    { { 0x348CECA0, 0x3FE57D69 } }, { { 0x992BFBB2, 0xBC875720 } },
    { { 0x5F037261, 0x3FE771E7 } }, { { 0x8D84068F, 0x3C75CFCE } },
    { { 0xBE65018C, 0x3FE5C77B } }, { { 0x9C0BC32A, 0x3C8069EA } },
    { { 0x37EFFF96, 0x3FE72D08 } }, { { 0x0F1D915C, 0x3C80D4EF } },
    { { 0x551D2CDF, 0x3FE610B7 } }, { { 0x52FF2A37, 0xBC7251B3 } },
    { { 0x54EAA8AF, 0x3FE6E744 } }, { { 0xC84E226E, 0xBC8DBC03 } },
    { { 0x25F0783D, 0x3FE65919 } }, { { 0xFBF5DE23, 0x3C8C3D64 } },
    { { 0x667F3BCD, 0x3FE6A09E } }, { { 0x13B26456, 0xBC8BDD34 } },
    { { 0x667F3BCD, 0x3FE6A09E } }, { { 0x13B26456, 0xBC8BDD34 } }
};

/* cos(pi * x), x=[0;1/512] */
static const FFTS_ALIGN(16) ffts_double_t cos_coeff[3] = {
    { { 0xC9BE45DE, 0xC013BD3C } },
    { { 0x081749FA, 0x40103C1F } },
    { { 0x047EE98B, 0xBFF55D10 } }
};

/* sin(pi * x), x=[0;1/512] */
static const FFTS_ALIGN(16) ffts_double_t sin_coeff[4] = {
    { { 0x54442D18, 0x400921FB } },
    { { 0xE62154CA, 0xC014ABBC } },
    { { 0xCEF16BFE, 0x40046675 } },
    { { 0xADE54A87, 0x40339228 } }
};

#ifndef M_1_256
#define M_1_256 3.90625e-3
#endif

static int
ffts_cexp_32f64f(size_t n, size_t d, double *out);

/* calculate cos(pi * n / d) and sin(pi * n / d) with maximum error less than 1 ULP, average ~0.5 ULP */
int
ffts_cexp_32f(size_t n, size_t d, float *output)
{
    double FFTS_ALIGN(16) z[2];

    if (!d || !output)
        return -1;

    /* reduction */
    if (FFTS_UNLIKELY(n >= d))
        n %= d;

    ffts_cexp_32f64f(n, d, z);

    output[0] = (float) z[0];
    output[1] = (float) z[1];
    return 0;
}

/* used as intermediate result for single precision calculations */
static int
ffts_cexp_32f64f(size_t n, size_t d, double *output)
{
    const ffts_double_t *ct = (const ffts_double_t*) FFTS_ASSUME_ALIGNED_32(cos_sin_table);
    const ffts_double_t *cc = (const ffts_double_t*) FFTS_ASSUME_ALIGNED_16(cos_coeff);
    const ffts_double_t *sc = (const ffts_double_t*) FFTS_ASSUME_ALIGNED_16(sin_coeff);
    double *out = FFTS_ASSUME_ALIGNED_16(output);
    double c, s, cos_a, cos_b, sin_a, sin_b;
    double cos_sign, sin_sign, sign, x, z;
    int i, j, swap;

    /* we know this */
    FFTS_ASSUME(d > 0);
    FFTS_ASSUME(n < d);

    /* determinate octant */
    if (n > d - n) {
        sin_sign = -1.0;
        n = d - n;
    } else {
        sin_sign = 1.0;
    }

    n <<= 1;
    if (n > d - n) {
        cos_sign = -1.0;
        swap = 1;
        n += n - d;
    } else {
        cos_sign = 1.0;
        swap = 0;
        n <<= 1;
    }

    if (n > d - n) {
        swap ^= 1;
        n = d - n;
    }

    /* "binary long division" */
    for (i = 0, j = (1 << 5), n <<= 1; j && n; j >>= 1) {
        if (n > d - n) {
            n += n - d;
            i += j;
        } else {
            n <<= 1;
        }
    }

    /* decide between two table values */
    if (n > d - n) {
        i++;
        n = d - n;
        sign = -1.0;
    } else {
        sign = 1.0;
    }

    /* divide by 256 is exact (as is the multiply with its reciprocal) */
    x = ((double) n / d) * M_1_256;

    /* 0 <= x <= 1/512 */
    z = x * x;

    /* table lookup */
    cos_a = ct[4 * i + 0].d;
    sin_a = ct[4 * i + 2].d;

    /* evaluate polynomials */
    cos_b = 1.0 + z * (cc[0].d + z * (cc[1].d + z * cc[2].d));
    sin_b = x * (sc[0].d + z * (sc[1].d + z * (sc[2].d + z * sc[3].d)));

    /* sum or difference of angles */
    c = cos_a * cos_b - sign * sin_a * sin_b;
    s = sin_a * cos_b + sign * cos_a * sin_b;

    if (swap) {
        double tmp = c;
        c = s;
        s = tmp;
    }

    out[0] = cos_sign * c;
    out[1] = sin_sign * s;
    return 0;
}

int
ffts_generate_chirp_32f(ffts_cpx_32f *const table, size_t table_size)
{
    ffts_cpx_32f *lut;
    size_t i, j, n;

    if (!table || !table_size)
        return -1;

    n = 2 * table_size;
    lut = ffts_aligned_malloc(n * sizeof(*lut));
    if (!lut)
        return -1;

    /* initialize LUT */
    ffts_generate_cosine_sine_32f(lut, n);

    /* generate CZT sequence */
    for (i = 0, j = 0; i < table_size; ++i) {
        table[i][0] = lut[j][0];
        table[i][1] = lut[j][1];

        j += 2 * i + 1;
        if (j >= n)
            j -= n;
    }

    ffts_aligned_free(lut);
    return 0;
}

/* generate cosine and sine tables with maximum error less than 1 ULP, average ~0.5 ULP
*  using repeated subvector scaling algorithm, 16 - 20 times faster than
*  direct library calling algorithm.
*/
int
ffts_generate_cosine_sine_32f(ffts_cpx_32f *const table, size_t table_size)
{
    ffts_cpx_64f *const tmp = (ffts_cpx_64f *const) table;
    double FFTS_ALIGN(16) z[2], zz[2], x[2], xx[2];
    size_t i, j, k, len;

    if (!table || !table_size)
        return -1;

    if (FFTS_UNLIKELY(table_size == 1))
        goto exit;

    /* check if table size is a power of two */
    if (!(table_size & (table_size - 1)))
        return ffts_generate_cosine_sine_pow2_32f(table, table_size);

    if (!(table_size & 1)) {
        /* even table size -- check if multiply of four */
        if (!(table_size & 3)) {
            /* multiply of four */
            len = table_size >> 2;
            for (j = 1; 4 * j <= len; j <<= 1) {
                ffts_cexp_32f64f(j, table_size, z);

                tmp[j][0] = z[0];
                tmp[j][1] = z[1];

                tmp[len - j][0] = z[1];
                tmp[len - j][1] = z[0];

                for (i = 1; i < j; i++) {
                    zz[0] = z[0] * tmp[i][0] - z[1] * tmp[i][1];
                    zz[1] = z[1] * tmp[i][0] + z[0] * tmp[i][1];

                    tmp[j + i][0] = zz[0];
                    tmp[j + i][1] = zz[1];

                    tmp[len - j - i][0] = zz[1];
                    tmp[len - j - i][1] = zz[0];
                }
            }

            /* this loops zero or one times */
            for (k = j << 1; k <= len; j <<= 1) {
                ffts_cexp_32f64f(j, table_size, z);

                tmp[j][0] = z[0];
                tmp[j][1] = z[1];
                if (k++ == len)
                    break;

                tmp[len - j][0] = z[1];
                tmp[len - j][1] = z[0];
                if (k++ == len)
                    break;

                for (i = 1; i < j; i++) {
                    zz[0] = z[0] * tmp[i][0] - z[1] * tmp[i][1];
                    zz[1] = z[1] * tmp[i][0] + z[0] * tmp[i][1];

                    tmp[j + i][0] = zz[0];
                    tmp[j + i][1] = zz[1];
                    if (k++ == len)
                        break;

                    tmp[len - j - i][0] = zz[1];
                    tmp[len - j - i][1] = zz[0];
                    if (k++ == len)
                        break;
                }
            }

            /* convert doubles to floats */
            for (i = 1; i < len; i++) {
                table[i][0] = (float) tmp[i][0];
                table[i][1] = (float) tmp[i][1];
            }

            table[len][0] = 0.0f;
            table[len][1] = 1.0f;

            for (i = 1; i <= len; i++) {
                table[len + i][0] = -table[i][1];
                table[len + i][1] =  table[i][0];
            }
        } else {
            /* multiply of two */
            len = table_size >> 1;
            for (j = 1; 4 * j <= len; j <<= 1) {
                ffts_cexp_32f64f(j, table_size, z);

                tmp[j][0] = z[0];
                tmp[j][1] = z[1];

                tmp[len - j][0] = -z[0];
                tmp[len - j][1] = z[1];

                for (i = 1; i < j; i++) {
                    zz[0] = z[0] * tmp[i][0] - z[1] * tmp[i][1];
                    zz[1] = z[1] * tmp[i][0] + z[0] * tmp[i][1];

                    tmp[j + i][0] = zz[0];
                    tmp[j + i][1] = zz[1];

                    tmp[len - j - i][0] = -zz[0];
                    tmp[len - j - i][1] = zz[1];
                }
            }

            /* this loops zero or one times */
            for (k = j << 1; k <= len; j <<= 1) {
                ffts_cexp_32f64f(j, table_size, z);

                tmp[j][0] = z[0];
                tmp[j][1] = z[1];
                if (k++ == len)
                    break;

                tmp[len - j][0] = -z[0];
                tmp[len - j][1] = z[1];
                if (k++ == len)
                    break;

                for (i = 1; i < j; i++) {
                    zz[0] = z[0] * tmp[i][0] - z[1] * tmp[i][1];
                    zz[1] = z[1] * tmp[i][0] + z[0] * tmp[i][1];

                    tmp[j + i][0] = zz[0];
                    tmp[j + i][1] = zz[1];
                    if (k++ == len)
                        break;

                    tmp[len - j - i][0] = -zz[0];
                    tmp[len - j - i][1] = zz[1];
                    if (k++ == len)
                        break;
                }
            }

            /* convert doubles to floats */
            for (i = 1; i < len; i++) {
                table[i][0] = (float) tmp[i][0];
                table[i][1] = (float) tmp[i][1];
            }

            table[len][0] = -1.0f;
            table[len][1] = 0.0f;
        }

        /* duplicate lower half to higher */
        len = table_size >> 1;
        for (i = 1; i < len; i++) {
            table[table_size - i][0] = table[i][0];
            table[table_size - i][1] = -table[i][1];
        }
    } else {
        /* odd table size */

        /* to avoid using temporary tables, generate the first 1/8 of table in
        *  double precision on lower half (and using the symmetry store
        *  the last 1/8 of table in single precision on higher half)
        */
        for (j = 1; 8 * j < table_size; j <<= 1) {
            ffts_cexp_32f64f(j, table_size, z);

            /* store double precision to lower half */
            tmp[j][0] = z[0];
            tmp[j][1] = z[1];

            /* store single precision to higher half */
            table[table_size - j][0] = (float) z[0];
            table[table_size - j][1] = (float) -z[1];

            for (i = 1; i < j; i++) {
                /* use double precision for intermediate calculations */
                zz[0] = z[0] * tmp[i][0] - z[1] * tmp[i][1];
                zz[1] = z[1] * tmp[i][0] + z[0] * tmp[i][1];

                tmp[i + j][0] = zz[0];
                tmp[i + j][1] = zz[1];

                table[table_size - i - j][0] = (float) zz[0];
                table[table_size - i - j][1] = (float) -zz[1];
            }
        }

        /* now generate 1/2 of table in single precision on higher half */
        k = j << 1;
        ffts_cexp_32f64f(j, table_size, z);
        ffts_cexp_32f64f(k, table_size, x);

        /* store single precision to higher half */
        table[table_size - j][0] = (float) z[0];
        table[table_size - j][1] = (float) -z[1];

        table[table_size - k][0] = (float) x[0];
        table[table_size - k][1] = (float) -x[1];

        i = 1;
        len = ((table_size + 1) >> 1) - k;
        if (len > j) {
            len -= j;

            xx[0] = x[0] * z[0] - x[1] * z[1];
            xx[1] = x[1] * z[0] + x[0] * z[1];

            table[table_size - k - j][0] = (float) xx[0];
            table[table_size - k - j][1] = (float) -xx[1];

            for (; i < len; i++) {
                zz[0] = z[0] * tmp[i][0] - z[1] * tmp[i][1];
                zz[1] = z[1] * tmp[i][0] + z[0] * tmp[i][1];

                table[table_size - i - j][0] = (float) zz[0];
                table[table_size - i - j][1] = (float) -zz[1];

                xx[0] = x[0] * tmp[i][0] - x[1] * tmp[i][1];
                xx[1] = x[1] * tmp[i][0] + x[0] * tmp[i][1];

                table[table_size - i - k][0] = (float) xx[0];
                table[table_size - i - k][1] = (float) -xx[1];

                xx[0] = x[0] * zz[0] - x[1] * zz[1];
                xx[1] = x[1] * zz[0] + x[0] * zz[1];

                table[table_size - i - k - j][0] = (float) xx[0];
                table[table_size - i - k - j][1] = (float) -xx[1];
            }

            len = j;
        }

        for (; i < len; i++) {
            zz[0] = z[0] * tmp[i][0] - z[1] * tmp[i][1];
            zz[1] = z[1] * tmp[i][0] + z[0] * tmp[i][1];

            table[table_size - i - j][0] = (float) zz[0];
            table[table_size - i - j][1] = (float) -zz[1];

            xx[0] = x[0] * tmp[i][0] - x[1] * tmp[i][1];
            xx[1] = x[1] * tmp[i][0] + x[0] * tmp[i][1];

            table[table_size - i - k][0] = (float) xx[0];
            table[table_size - i - k][1] = (float) -xx[1];
        }

        for (; i < j; i++) {
            zz[0] = z[0] * tmp[i][0] - z[1] * tmp[i][1];
            zz[1] = z[1] * tmp[i][0] + z[0] * tmp[i][1];

            table[table_size - i - j][0] = (float) zz[0];
            table[table_size - i - j][1] = (float) -zz[1];
        }

        /* duplicate higher half to lower */
        len = table_size >> 1;
        for (i = 1; i <= len; i++) {
            table[i][0] = table[table_size - i][0];
            table[i][1] = -table[table_size - i][1];
        }
    }

exit:
    /* cos(0) = 1.0, sin(0) = 0.0 */
    table[0][0] = 1.0f;
    table[0][1] = 0.0f;
    return 0;
}

/* Oscar Buneman's method for generating a sequence of sines and cosines.
*  Expired US Patent 4,878,187 A
*/
#if HAVE_SSE2
int
ffts_generate_cosine_sine_pow2_32f(ffts_cpx_32f *const table, int table_size)
{
    static const __m128d sign_swap = { 0.0, -0.0 };
    const __m128d *FFTS_RESTRICT ct;
    const ffts_double_t *FFTS_RESTRICT hs;
    __m128d FFTS_ALIGN(16) w[32];
    __m128d FFTS_ALIGN(16) h[32];
    int i, log_2, offset;

    /* size must be a power of two */
    if (!table || !table_size || (table_size & (table_size - 1))) {
        return -1;
    }

    /* the first */
    table[0][0] =  1.0f;
    table[0][1] = -0.0f;

    if (FFTS_UNLIKELY(table_size == 1)) {
        goto exit;
    }

    if (FFTS_UNLIKELY(table_size == 2)) {
        /* skip over */
        i = 1;
        goto mid_point;
    }

    /* calculate table offset */
    FFTS_ASSUME(table_size/2 > 1);
    log_2 = ffts_ctzl(table_size);
    FFTS_ASSUME(log_2 > 1);
    offset = 32 - log_2;
    ct = (const __m128d*)
        FFTS_ASSUME_ALIGNED_32(&cos_sin_pi_table[4 * offset]);
    hs = FFTS_ASSUME_ALIGNED_16(&half_secant[2 * offset]);

    /* initialize from lookup table */
    for (i = 0; i <= log_2; i++) {
        w[i] = ct[2*i];

        /* duplicate the high part */
        h[i] = _mm_set1_pd(hs[2*i].d);
    }

    /* generate sine and cosine tables with maximum error less than 0.5 ULP */
    for (i = 1; i < table_size/2; i++) {
        /* calculate trailing zeros in index */
        log_2 = ffts_ctzl(i);

        /* note that storing is not 16 byte aligned */
        _mm_storel_pi((__m64*) &table[i + 0],
            _mm_cvtpd_ps(_mm_or_pd(w[log_2], sign_swap)));
        _mm_storel_pi((__m64*) &table[table_size - i], _mm_cvtpd_ps(
            _mm_or_pd(_mm_shuffle_pd(w[log_2], w[log_2], 1), sign_swap)));

        /* skip and find next trailing zero */
        offset = (log_2 + 2 + ffts_ctzl(~i >> (log_2 + 2)));
        w[log_2] = _mm_mul_pd(h[log_2], _mm_add_pd(w[log_2 + 1], w[offset]));
    }

mid_point:
    table[i][0] =  0.70710677f;
    table[i][1] = -0.70710677f;

exit:
    return 0;
}

int
ffts_generate_cosine_sine_pow2_64f(ffts_cpx_64f *const table, int table_size)
{
    static const __m128d sign_swap = { 0.0, -0.0 };
    const struct ffts_dd2_t *FFTS_RESTRICT ct;
    const ffts_double_t *FFTS_RESTRICT hs;
    struct ffts_dd2_t FFTS_ALIGN(16) w[32];
    struct ffts_dd2_t FFTS_ALIGN(16) h[32];
    struct ffts_dd2_t FFTS_ALIGN(16) sum;
    int i, log_2, offset;

    /* size must be a power of two */
    if (!table || !table_size || (table_size & (table_size - 1))) {
        return -1;
    }

    /* the first */
    table[0][0] =  1.0;
    table[0][1] = -0.0;

    if (FFTS_UNLIKELY(table_size == 1)) {
        goto exit;
    }

    if (FFTS_UNLIKELY(table_size == 2)) {
        /* skip over */
        i = 1;
        goto mid_point;
    }

    /* calculate table offset */
    FFTS_ASSUME(table_size/2 > 1);
    log_2 = ffts_ctzl(table_size);
    FFTS_ASSUME(log_2 > 1);
    offset = 32 - log_2;
    ct = (const struct ffts_dd2_t*)
        FFTS_ASSUME_ALIGNED_32(&cos_sin_pi_table[4 * offset]);
    hs = FFTS_ASSUME_ALIGNED_16(&half_secant[2 * offset]);

    /* initialize from lookup table */
    for (i = 0; i <= log_2; i++) {
        w[i] = ct[i];

        /* duplicate the high and low parts */
        h[i].hi = _mm_set1_pd(hs[2*i + 0].d);
        h[i].lo = _mm_set1_pd(hs[2*i + 1].d);
    }

    /* generate sine and cosine tables with maximum error less than 0.5 ULP */
    for (i = 1; i < table_size/2; i++) {
        /* calculate trailing zeros in index */
        log_2 = ffts_ctzl(i);

        /* result of ffts_dd_mul_dd is normalized */
        _mm_store_pd((double*) &table[i + 0],
            _mm_or_pd(w[log_2].hi, sign_swap));
        _mm_store_pd((double*) &table[table_size - i],
            _mm_or_pd(_mm_shuffle_pd(w[log_2].hi, w[log_2].hi, 1), sign_swap));

        /* skip and find next trailing zero */
        offset = (log_2 + 2 + ffts_ctzl(~i >> (log_2 + 2)));
        sum = ffts_dd2_add_dd2_unnormalized(&w[log_2 + 1], &w[offset]);
        w[log_2] = ffts_dd2_mul_dd2(&h[log_2], &sum);
    }

mid_point:
    table[i][0] =  0.707106781186547524;
    table[i][1] = -0.707106781186547524;

exit:
    return 0;
}
#else
int
ffts_generate_cosine_sine_pow2_32f(ffts_cpx_32f *const table, int table_size)
{
    const ffts_cpx_64f *FFTS_RESTRICT ct;
    const double_t *FFTS_RESTRICT hs;
    ffts_cpx_64f FFTS_ALIGN(16) w[32];
    int i, log_2, offset;

    /* size must be a power of two */
    if (!table || !table_size || (table_size & (table_size - 1))) {
        return -1;
    }

    /* the first */
    table[0][0] =  1.0f;
    table[0][1] = -0.0f;

    if (FFTS_UNLIKELY(table_size == 1)) {
        goto exit;
    }

    if (FFTS_UNLIKELY(table_size == 2)) {
        /* skip over */
        i = 1;
        goto mid_point;
    }

    /* calculate table offset */
    FFTS_ASSUME(table_size/2 > 1);
    log_2 = ffts_ctzl(table_size);
    FFTS_ASSUME(log_2 > 1);
    offset = 32 - log_2;
    ct = (const ffts_cpx_64f*)
        FFTS_ASSUME_ALIGNED_32(&cos_sin_pi_table[4 * offset]);
    hs = FFTS_ASSUME_ALIGNED_16(&half_secant[2 * offset]);

    /* initialize from lookup table */
    for (i = 0; i <= log_2; i++) {
        w[i][0] = ct[2*i][0];
        w[i][1] = ct[2*i][1];
    }

    /* generate sine and cosine tables with maximum error less than 0.5 ULP */
    for (i = 1; i < table_size/2; i++) {
        /* calculate trailing zeros in index */
        log_2 = ffts_ctzl(i);

        table[i          + 0][0] = (float)  w[log_2][0];
        table[i          + 0][1] = (float) -w[log_2][1];
        table[table_size - i][0] = (float)  w[log_2][1];
        table[table_size - i][1] = (float) -w[log_2][0];

        /* skip and find next trailing zero */
        offset = (log_2 + 2 + ffts_ctzl(~i >> (log_2 + 2)));
        w[log_2][0] = hs[2 * log_2].d * (w[log_2 + 1][0] + w[offset][0]);
        w[log_2][1] = hs[2 * log_2].d * (w[log_2 + 1][1] + w[offset][1]);
    }

mid_point:
    table[i][0] =  0.70710677f;
    table[i][1] = -0.70710677f;

exit:
    return 0;
}

int
ffts_generate_cosine_sine_pow2_64f(ffts_cpx_64f *const table, int table_size)
{
    const struct ffts_dd_t *FFTS_RESTRICT ct;
    const struct ffts_dd_t *FFTS_RESTRICT hs;
    struct ffts_dd_t FFTS_ALIGN(16) w[32][2];
    int i, log_2, offset;

    /* size must be a power of two */
    if (!table || !table_size || (table_size & (table_size - 1))) {
        return -1;
    }

    /* the first */
    table[0][0] =  1.0;
    table[0][1] = -0.0;

    if (FFTS_UNLIKELY(table_size == 1)) {
        goto exit;
    }

    if (FFTS_UNLIKELY(table_size == 2)) {
        /* skip over */
        i = 1;
        goto mid_point;
    }

    /* calculate table offset */
    FFTS_ASSUME(table_size/2 > 1);
    log_2 = ffts_ctzl(table_size);
    FFTS_ASSUME(log_2 > 1);
    offset = 32 - log_2;
    ct = (const struct ffts_dd_t*)
        FFTS_ASSUME_ALIGNED_32(&cos_sin_pi_table[4 * offset]);
    hs = (const struct ffts_dd_t*) &half_secant[2 * offset];

    /* initialize from lookup table */
    for (i = 0; i <= log_2; i++) {
        w[i][0].hi = ct[2*i + 0].hi;
        w[i][0].lo = ct[2*i + 1].hi;
        w[i][1].hi = ct[2*i + 0].lo;
        w[i][1].lo = ct[2*i + 1].lo;
    }

    /* generate sine and cosine tables with maximum error less than 0.5 ULP */
    for (i = 1; i < table_size/2; i++) {
        /* calculate trailing zeros in index */
        log_2 = ffts_ctzl(i);

        /* result of ffts_dd_mul_dd is normalized */
        table[i          + 0][0] =  w[log_2][0].hi;
        table[i          + 0][1] = -w[log_2][1].hi;
        table[table_size - i][0] =  w[log_2][1].hi;
        table[table_size - i][1] = -w[log_2][0].hi;

        /* skip and find next trailing zero */
        offset = (log_2 + 2 + ffts_ctzl(~i >> (log_2 + 2)));
        w[log_2][0] = ffts_dd_mul_dd(hs[log_2],
            ffts_dd_add_dd_unnormalized(w[log_2 + 1][0], w[offset][0]));
        w[log_2][1] = ffts_dd_mul_dd(hs[log_2],
            ffts_dd_add_dd_unnormalized(w[log_2 + 1][1], w[offset][1]));
    }

mid_point:
    table[i][0] =  0.707106781186547524;
    table[i][1] = -0.707106781186547524;

exit:
    return 0;
}
#endif

int
ffts_generate_table_1d_real_32f(struct _ffts_plan_t *const p,
                                int sign,
                                int invert)
{
    const ffts_cpx_64f *FFTS_RESTRICT ct;
    const ffts_double_t *FFTS_RESTRICT hs;
    ffts_cpx_64f FFTS_ALIGN(16) w[32];
    int i, log_2, offset, N;
    float *A, *B;

    if (!p) {
        return -1;
    }

    A = (float*) FFTS_ASSUME_ALIGNED_32(p->A);
    B = (float*) FFTS_ASSUME_ALIGNED_32(p->B);
    N = (int) p->N;

    /* the first */
    if (sign < 0) {
        A[0] =  0.5f;
        A[1] = -0.5f;
        B[0] =  invert ? -0.5f : 0.5f;
        B[1] =  0.5f;
    } else {
        /* peel of the first */
        A[0] = 1.0f;
        A[1] = invert ? 1.0f : -1.0f;
        B[0] = 1.0f;
        B[1] = 1.0f;
    }

    if (FFTS_UNLIKELY(N == 4)) {
        i = 1;
        goto last;
    }

    /* calculate table offset */
    FFTS_ASSUME(N / 4 > 1);
    log_2 = ffts_ctzl(N);
    FFTS_ASSUME(log_2 > 2);
    offset = 34 - log_2;
    ct = (const ffts_cpx_64f*)
        FFTS_ASSUME_ALIGNED_32(&cos_sin_pi_table[4 * offset]);
    hs = FFTS_ASSUME_ALIGNED_16(&half_secant[2 * offset]);

    /* initialize from lookup table */
    for (i = 0; i <= log_2; i++) {
        w[i][0] = ct[2*i][0];
        w[i][1] = ct[2*i][1];
    }

    if (sign < 0) {
        for (i = 1; i < N/4; i++) {
            float t0, t1, t2; 

            /* calculate trailing zeros in index */
            log_2 = ffts_ctzl(i);

            t0 = (float) (0.5 * (1.0 - w[log_2][1]));
            t1 = (float) (0.5 * w[log_2][0]);
            t2 = (float) (0.5 * (1.0 + w[log_2][1]));

            A[    2 * i + 0] =  t0;
            A[N - 2 * i + 0] =  t0;
            A[    2 * i + 1] = -t1;
            A[N - 2 * i + 1] =  t1;

            B[    2 * i + 0] =  invert ? -t2 : t2;
            B[N - 2 * i + 0] =  invert ? -t2 : t2;
            B[    2 * i + 1] =  t1;
            B[N - 2 * i + 1] = -t1;

            /* skip and find next trailing zero */
            offset = (log_2 + 2 + ffts_ctzl(~i >> (log_2 + 2)));
            w[log_2][0] = hs[2 * log_2].d * (w[log_2 + 1][0] + w[offset][0]);
            w[log_2][1] = hs[2 * log_2].d * (w[log_2 + 1][1] + w[offset][1]);
        }
    } else {
        for (i = 1; i < N/4; i++) {
            float t0, t1, t2; 

            /* calculate trailing zeros in index */
            log_2 = ffts_ctzl(i);

            t0 = (float) (1.0 - w[log_2][1]);
            t1 = (float) w[log_2][0];
            t2 = (float) (1.0 + w[log_2][1]);

            A[    2 * i + 0] = t0;
            A[N - 2 * i + 0] = t0;
            A[    2 * i + 1] = invert ?  t1 : -t1;
            A[N - 2 * i + 1] = invert ? -t1 :  t1;

            B[    2 * i + 0] =  t2;
            B[N - 2 * i + 0] =  t2;
            B[    2 * i + 1] =  t1;
            B[N - 2 * i + 1] = -t1;

            /* skip and find next trailing zero */
            offset = (log_2 + 2 + ffts_ctzl(~i >> (log_2 + 2)));
            w[log_2][0] = hs[2 * log_2].d * (w[log_2 + 1][0] + w[offset][0]);
            w[log_2][1] = hs[2 * log_2].d * (w[log_2 + 1][1] + w[offset][1]);
        }
    }

last:
    if (sign < 0) {
        A[2 * i + 0] = 0.0f;
        A[2 * i + 1] = 0.0f;
        B[2 * i + 0] = invert ? -1.0f : 1.0f;
        B[2 * i + 1] = 0.0f;
    } else {
        A[2 * i + 0] = 0.0f;
        A[2 * i + 1] = 0.0f;
        B[2 * i + 0] = 2.0f;
        B[2 * i + 1] = 0.0f;
    }

    return 0;
}
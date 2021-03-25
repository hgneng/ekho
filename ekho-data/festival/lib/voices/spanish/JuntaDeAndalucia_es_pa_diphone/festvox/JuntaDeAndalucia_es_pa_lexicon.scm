;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                                                                     ;;;
;;;                      JUNTA DE ANDALUCÍA                             ;;;
;;;                      Copyright (c) 2007                             ;;;
;;;                      All Rights Reserved.                           ;;;
;;;                                                                     ;;;
;;;  Distribution policy.                                               ;;;
;;;                                                                     ;;;
;;;  Free for any use.                                                  ;;;
;;;                                                                     ;;;
;;;  All the work is based on the Festvox Toolkit, provided by:         ;;;
;;;    - Carnegie Mellon University (http://www.festvox.org)            ;;;
;;;                                                                     ;;;
;;;  The copyright below belongs to the original Festvox project; it    ;;;
;;;  therefore applies to the present work.                             ;;;
;;;                                                                     ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                                                                     ;;;
;;;  Standard Spanish male voice                                        ;;;
;;;                                                                     ;;;
;;;  Contractor: Consejería de Innovación, Ciencia y Empresa            ;;;
;;;              de la Junta de Andalucía                               ;;;
;;;                                                                     ;;;
;;;  Developed by: MP Sistemas and                                      ;;;
;;;                Intelligent Dialogue Systems S.L. (INDISYS)          ;;;
;;;                                                                     ;;;
;;;  Authors:   Del Solar, Carmen <c.delsolar@indisys.es>               ;;;
;;;             González, Jesús   <j.gonzalez@indisys.es>               ;;;
;;;             Manchón, Pilar    <p.manchon@indisys.es>                ;;;
;;;             Martín, Antonio   <amam@mpsistemas.es>                  ;;;
;;;             Martínez, Diego   <d.martinez@indisys.es>               ;;;
;;;             Pérez, Guillermo  <g.perez@indisys.es>                  ;;;
;;;             Varela, Víctor    <vmvr@mpsistemas.es>                  ;;;
;;;                                                                     ;;;
;;;  Voice Talent:  Pedro Alonso                                        ;;;
;;;                                                                     ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                                                                     ;;;
;;;                     Carnegie Mellon University                      ;;;
;;;                  and Alan W Black and Kevin Lenzo                   ;;;
;;;                      Copyright (c) 1998-2000                        ;;;
;;;                        All Rights Reserved.                         ;;;
;;;                                                                     ;;;
;;; Permission is hereby granted, free of charge, to use and distribute ;;;
;;; this software and its documentation without restriction, including  ;;;
;;; without limitation the rights to use, copy, modify, merge, publish, ;;;
;;; distribute, sublicense, and/or sell copies of this work, and to     ;;;
;;; permit persons to whom this work is furnished to do so, subject to  ;;;
;;; the following conditions:                                           ;;;
;;;  1. The code must retain the above copyright notice, this list of   ;;;
;;;     conditions and the following disclaimer.                        ;;;
;;;  2. Any modifications must be clearly marked as such.               ;;;
;;;  3. Original authors' names are not deleted.                        ;;;
;;;  4. The authors' names are not used to endorse or promote products  ;;;
;;;     derived from this software without specific prior written       ;;;
;;;     permission.                                                     ;;;
;;;                                                                     ;;;
;;; CARNEGIE MELLON UNIVERSITY AND THE CONTRIBUTORS TO THIS WORK        ;;;
;;; DISCLAIM ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING     ;;;
;;; ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO EVENT  ;;;
;;; SHALL CARNEGIE MELLON UNIVERSITY NOR THE CONTRIBUTORS BE LIABLE     ;;;
;;; FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES   ;;;
;;; WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN  ;;;
;;; AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION,         ;;;
;;; ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF      ;;;
;;; THIS SOFTWARE.                                                      ;;;
;;;                                                                     ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Addenda and hand-written LTS rules for JuntaDeAndalucia_es
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Load any necessary files here

(define (JuntaDeAndalucia_es_addenda)
  "(JuntaDeAndalucia_es_addenda)
Basic lexicon should (must ?) have basic letters, symbols and punctuation."

   ;;;Specific terms, acronyms, abbreviations, etc.
(lex.add.entry '("2d" nn (((d o s) 0)((d e1) 1))))
(lex.add.entry '("2g" nn (((d o1 s) 1)((x e1) 1))))
(lex.add.entry '("3d" nn (((t r e s) 0)((d e1) 1))))
(lex.add.entry '("3g" nn (((t r e1 s) 1)((x e1) 1))))
(lex.add.entry '("abnf" nn (((a1) 1)((B e) 0)((e1) 1)((n e) 0)((e1) 1)((f e) 0))))
(lex.add.entry '("access" nn (((a1 k) 1) ((s e s) 0))))
(lex.add.entry '("acrobat" nn (((a) 0)((k r o1) 1)((B a t) 0))))
(lex.add.entry '("activex" nn (((a1 k) 1)((t i v) 0)((e1) 1)((k i s) 0))))
(lex.add.entry '("adn" nn (((a1) 1)((D e1) 1)((e1) 1)((n e) 0))))
(lex.add.entry '("adobe" nn (((a) 0)((D o1) 1)((B e) 0))))
(lex.add.entry '("adsl" nn (((a) 0)((D e) 0)((e) 0)((s e)0)((e1) 1)((l e) 0))))
(lex.add.entry '("aisleriot" nn (((a i l) 0)((rr a1 i o t) 1))))
(lex.add.entry '("amazon" nn (((a1) 1)((m a) 0)((th o n) 0))))
(lex.add.entry '("am" nn (((a) 0)((e1) 1)((m e) 0))))
(lex.add.entry '("amsn" nn (((a) 0)((e1) 1)((m e) 0)((e1) 1)((s e) 0)((e1) 1)((n e) 0))))
(lex.add.entry '("amule" nn (((a) 0)((m u1) 1)((l e) 0))))
(lex.add.entry '("apple" nn (((a1) 1)((p e l) 0))))
(lex.add.entry '("applet" nn (((a1) 1)((p l e t) 0))))
(lex.add.entry '("applets" nn (((a1) 1)((p l e t s) 0))))
(lex.add.entry '("ataxx" nn (((a) 0)((t a1 k s) 1))))
(lex.add.entry '("audacity" nn (((o) 0)((D a1) 1)((s i) 0)((t i) 0))))
(lex.add.entry '("avidemux" nn (((a) 0)((v i) 0)((D e1) 1)((m u k s) 0))))
(lex.add.entry '("backend" nn (((b a k) 0)((e1 n d) 1))))
(lex.add.entry '("backup" nn (((b a) 0)((k a1 p) 1))))
(lex.add.entry '("banner" nn (((b a1) 1)((n ax r) 0))))
(lex.add.entry '("banners" nn (((b a1) 1)((n ax r s) 0))))
(lex.add.entry '("banshee" nn (((b a1 n) 1)((sh i) 0))))
(lex.add.entry '("basket" nn (((b a1 s) 1)((k e t) 0))))
(lex.add.entry '("bbc" nn (((b e1) 1)((B e1) 1)((th e1) 1))))
(lex.add.entry '("beagle" nn (((b i1) 1)((G ax l) 0))))
(lex.add.entry '("bitrate" nn (((b i t) 0)((rr e1 i t) 1))))
(lex.add.entry '("blackjack" nn (((b l a1 k) 1)((ll a k) 0))))
(lex.add.entry '("blam" nn (((b l a1 m) 1))))
(lex.add.entry '("blender" nn (((b l e1 n) 1)((d ax r) 0))))
(lex.add.entry '("blog" nn (((b l o1 g) 1))))
(lex.add.entry '("bluefish" nn (((b l u1) 1)((f i sh) 0))))
(lex.add.entry '("bluetooth" nn (((b l u1) 1)((t u th) 0))))
(lex.add.entry '("bmp" nn (((b e1) 1)((e1) 1)((m e) 0)((p e1) 1))))
(lex.add.entry '("bnf" nn (((b e1) 1)((e1) 1)((n e) 0)((e1) 1)((f e) 0))))
(lex.add.entry '("boot" nn (((b u1 t) 1))))
(lex.add.entry '("braille" nn (((b r a1 i) 1)((l e) 0))))
(lex.add.entry '("browser" nn (((b r o1 u) 1)((s ax r) 0))))
(lex.add.entry '("btdownload" nn (((b e1) 1)((t e) 0)((D a1 u n) 1)((l o u d) 0))))
(lex.add.entry '("bubble" nn (((b a1) 1)((b ax l) 0))))
(lex.add.entry '("button" nn (((b a1) 1)((t o n) 0))))
(lex.add.entry '("byte" nn (((b a1 i t) 1))))
(lex.add.entry '("bytes" nn (((b a1 i t s) 1))))
(lex.add.entry '("byzanz" nn (((b a1 i) 1)((z a n s) 0))))
(lex.add.entry '("calendar" nn (((k a1) 1)((l e n) 0)((d a r) 0))))
(lex.add.entry '("carnegie" nn (((k a1 r) 1)((n e) 0)((g i) 0))))
(lex.add.entry '("cd" nn (((th e) 0)((D e1) 1))))
(lex.add.entry '("cdrom" nn (((th e1) 1)((D e) 0)((rr o1 m) 1))))
(lex.add.entry '("cdrw" nn (((th e) 1)((D e1) 1)((e1) 1)((rr e) 0)((u1) 1)((B e) 0)((D o1) 1)((b l e) 0))))
(lex.add.entry '("cds" nn (((th e) 0)((D e1 s) 1))))
(lex.add.entry '("cd's" nn (((th e) 0)((D e1 s) 1))))
(lex.add.entry '("cgi" nn (((th e) 0)((x e1) 1)((i) 0))))
(lex.add.entry '("checked" nn (((ch e1 k t) 1))))
(lex.add.entry '("cni" nn (((th e1) 1)((e1) 1)((n e) 0)((i1) 1))))
(lex.add.entry '("cnp" nn (((th e) 1)((e1) 1)((n e) 0)((p e1) 1))))
(lex.add.entry '("composer" nn (((k o m) 0)((p o1 u) 1)((s ax r) 0))))
(lex.add.entry '("cookie" nn (((k u1) 1)((k i) 0))))
(lex.add.entry '("cookies" nn (((k u1) 1)((k i s) 0))))
(lex.add.entry '("copyright" nn (((k o1) 1)((p i) 0)((rr a i t) 0))))
(lex.add.entry '("copywright" nn (((k o1) 1)((p i) 0)((rr a i t) 0))))
(lex.add.entry '("cracker" nn (((k r a1) 1)((k ax r) 0))))
(lex.add.entry '("crackers" nn (((k r a1) 1)((k ax r s) 0))))
(lex.add.entry '("crn" nn (((th e) 0) ((e1) 1) ((rr e) 0) ((e1) 1) ((n e) 0))))
(lex.add.entry '("csic" nn (((th e) 1)((s i1 k) 1))))
(lex.add.entry '("csif" nn (((th e) 1)((s i1 f) 1))))
(lex.add.entry '("css" nn (((th e) 0)((e1) 1)((s e) 0)((e1) 1)((s e) 0))))
(lex.add.entry '("ctrl" nn (((k o n) 0)((t r o1 l) 1))))
(lex.add.entry '("cvs" nn (((th e1) 1)((u) 0)((B e) 0)((e1) 1)((s e) 0))))
(lex.add.entry '("d4x" nn (((d e) 0)((k u1 a) 1)((t r o) 0)((e1) 1)((k i s) 0))))
(lex.add.entry '("dasher" nn (((d a1) 1)((sh ax r) 0))))
(lex.add.entry '("desktop" nn (((d e1 s k) 1)((t o p) 0))))
(lex.add.entry '("device" nn (((d i) 0)((v a1 i s) 1))))
(lex.add.entry '("dgt" nn (((d e1) 1)((x e1) 1)((t e1) 1))))
(lex.add.entry '("dhtml" nn (((d e1) 1)((a) 0)((ch e) 0)((t e) 0)((e)0)((m e) 0)((e1) 1)((l e) 0))))
(lex.add.entry '("dialogue" nn (((d a1 i) 1)((a) 0)((l o g) 0))))
(lex.add.entry '("dictionary" nn (((d i1 k) 1)((sh o) 0)((n a) 0)((r i) 0))))
(lex.add.entry '("digicam" nn (((d i) 0)((zh i) 0)((k a1 m) 1))))
(lex.add.entry '("digikam" nn (((d i) 0)((zh i) 0)((k a1 m) 1))))
(lex.add.entry '("dll" nn (((d e1) 1)((e1) 1)((l e) 0)((e1) 1)((l e) 0))))
(lex.add.entry '("dni" nn (((d e1) 1)((e1) 1)((n e) 0)((i1) 1))))
(lex.add.entry '("dns" nn (((d e) 0)((e1) 1)((n e) 0)((e) 0)((s e) 0))))
(lex.add.entry '("dollar" nn (((d o1) 1)((l a r) 0))))
(lex.add.entry '("draw" nn (((d r o1) 1))))
(lex.add.entry '("drivel" nn (((d e) 0)((r a1 i) 1)((v ax l) 0))))
(lex.add.entry '("drivers" nn (((d r a1 i) 1)((v ax r s) 0))))
(lex.add.entry '("drm" nn (((d e) 0)((e1) 1)((rr e) 0)((e) 0)((m e) 0))))
(lex.add.entry '("drv" nn (((d e1) 1)((e1) 1)((rr e) 0)((u1) 1)((B e) 0))))
(lex.add.entry '("dvd" nn (((d e) 0)((u) 0)((B e) 0)((D e1) 1))))
(lex.add.entry '("dvds" nn (((d e) 0)((u) 0)((B e) 0)((D e1 s) 1))))
(lex.add.entry '("dvd's" nn (((d e) 0)((u) 0)((B e) 0)((D e1 s) 1))))
(lex.add.entry '("dvi" nn (((d e) 0)((u1) 1)((B e) 0)((i) 0))))
(lex.add.entry '("ebay" nn (((i1) 1)((B e i) 0))))
(lex.add.entry '("edimburgh" nn (((a1) 1)((D i m) 0)((b r a) 0))))
(lex.add.entry '("eeuu" nn (((e s) 0)((t a1) 1)((D o s) 0)((u) 0)((n i1) 1)((D o s) 0))))
(lex.add.entry '("ekiga" nn (((e) 0)((k i1) 1)((g a) 0))))
(lex.add.entry '("emacsspeak" nn (((i1) 1)((m a k) 0)((s p i1 k) 1))))
(lex.add.entry '("email" nn (((i) 0)((m e1 i l) 1))))
(lex.add.entry '("emails" nn (((i) 0)((m e1 i l s) 1))))
(lex.add.entry '("enter" nn (((e1 n) 1)((t ax r) 0))))
(lex.add.entry '("epiphany" nn (((e) 0) ((p i1) 1)((f a) 0)((n i) 0))))
(lex.add.entry '("etc" nn (((e t) 0)((th e1) 1)((t e) 0)((r a) 0))))
(lex.add.entry '("evince" nn (((i) 0)((v i1 n s) 1))))
(lex.add.entry '("evolution" nn (((e) 0)((v o) 0)((l u1) 1)((sh ax n) 0))))
(lex.add.entry '("explorer" nn (((e k s) 0)((p l o1) 1)((r ax r) 0))))
(lex.add.entry '("fade" nn (((f e1 i d) 1))))
(lex.add.entry '("fbi" nn (((e1) 1)((f e) 0)((B e1) 1)((i1) 1))))
(lex.add.entry '("ffcc" nn (((f e) 0)((rr o) 0)((k a) 0)((rr i1) 1)((l e s) 0))))
(lex.add.entry '("file" nn (((f a1 i l) 1))))
(lex.add.entry '("finger" nn (((f i1 n) 1)((g e r) 0))))
(lex.add.entry '("firefox" nn (((f a1 i r) 1)((f o k s) 0))))
(lex.add.entry '("firewall" nn (((f a1 i r) 1)((u o l) 0))))
(lex.add.entry '("firewalls" nn (((f a1 i r) 1)((u o l s) 0))))
(lex.add.entry '("flash" nn (((f l a1 sh) 1))))
(lex.add.entry '("fm" nn (((e1) 1)((f e) 0)((e1) 1)((m e) 0))))
(lex.add.entry '("fp" nn (((e1) 1)((f e) 0)((p e1) 1))))
(lex.add.entry '("frame" nn (((f r e1 i m) 1))))
(lex.add.entry '("frames" nn (((f r e1 i m s) 1))))
(lex.add.entry '("freecell" nn (((f r i1) 1)((s e l) 0))))
(lex.add.entry '("freemind" nn (((f r i1) 1)((m a i n d) 0))))
(lex.add.entry '("freeware" nn (((f r i1) 1)((u a r) 0))))
(lex.add.entry '("frozen" nn (((f r o1 u) 1)((z ax n) 0))))
(lex.add.entry '("fspot" nn (((e1) 1)((f e) 0)((s p o1 t) 1))))
(lex.add.entry '("ftp" nn (((e1) 1)((f e) 0)((t e) 0)((p e1) 1))))
(lex.add.entry '("gaim" nn (((g e1 i m) 1))))
(lex.add.entry '("gateway" nn (((g e1 i t) 1)((u e i) 0))))
(lex.add.entry '("gbeep" nn (((x e) 0)((B i1 p) 1))))
(lex.add.entry '("gdesklets" nn (((x e1) 1) ((D e1 s k) 1)((l e t s) 0))))
(lex.add.entry '("gedit" nn (((x e) 0)((e1) 1)((D i t) 0))))
(lex.add.entry '("gftp" nn (((x e) 0)((e1) 1)((f e) 0)((t e1) 1)((p e) 0))))
(lex.add.entry '("gfxboot" nn (((x e1) 1)((e) 0)((f e) 0)((e1) 1)((k i s) 0)((b u1 t) 1))))
(lex.add.entry '("gif" nn (((g i1 f) 1))))
(lex.add.entry '("gigabyte" nn (((x i) 0)((G a) 0)((B a1 i t) 1))))
(lex.add.entry '("gigabytes" nn (((x i) 0)((G a) 0)((B a1 i t s) 1))))
(lex.add.entry '("gimp" nn (((g i1 m p) 1))))
(lex.add.entry '("gksudo" nn (((x e1) 1)((k a1) 1)((s u1) 1)((D o) 0))))
(lex.add.entry '("glade" nn (((g l e1 i d) 1))))
(lex.add.entry '("gmail" nn (((x e) 0)((m e1 i l) 1))))
(lex.add.entry '("gmt" nn (((x e) 1)((e1) 1)((m e) 0)((t e1) 1))))
(lex.add.entry '("gnomebaker" nn (((x e) 0)((n o1) 1)((m e) 0)((B e1 i) 1)((k ax r) 0))))
(lex.add.entry '("gnome" nn (((x e) 0)((n o1) 1)((m e)0))))
(lex.add.entry '("gnometris" nn (((x e) 0)((n o) 0)((m e1) 1)((t r i s) 0))))
(lex.add.entry '("gnopernicus" nn (((x e) 0)((n o) 0)((p e1 r) 1)((n i) 0)((k u s) 0))))
(lex.add.entry '("gnucash" nn (((x e) 0)((e1) 1)((n e) 0)((u1) 1)((k a sh) 1))))
(lex.add.entry '("gnu" nn (((x e) 0)((e1) 1)((n e) 0)((u) 1))))
(lex.add.entry '("google" nn (((g u1) 1)((G ax l) 0))))
(lex.add.entry '("gpl" nn (((x e) 0)((p e) 0)((e1) 1)((l e) 0))))
(lex.add.entry '("gprs" nn (((x e1) 1)((p e1) 1)((e1) 1)((rr e) 0)((e1) 1)((s e) 0))))
(lex.add.entry '("gps" nn (((x e1) 1)((p e1) 1)((e1) 1)((s e) 0))))
(lex.add.entry '("grip" nn (((g r i1 p) 1))))
(lex.add.entry '("grub" nn (((g r a1 b) 1))))
(lex.add.entry '("gshare" nn (((x e) 0)((sh e1 a r) 1))))
(lex.add.entry '("gstreamer" nn (((x e) 0)((s t r i1) 1)((m ax r) 0))))
(lex.add.entry '("gthumb" nn (((x e) 0)((th a1 m b) 1))))
(lex.add.entry '("gtk" nn (((x e) 0)((t e1) 1)((k a) 0))))
(lex.add.entry '("gui" nn (((g u1 i) 1))))
(lex.add.entry '("gxine" nn (((x e k) 0)((s a1 i n) 1))))
(lex.add.entry '("gzip" nn (((x e1) 1)((th i1 p) 1))))
(lex.add.entry '("gz" nn (((x e1) 1)((th e1) 1)((t a) 0))))
(lex.add.entry '("hacker" nn (((hh a1) 1)((k ax r) 0))))
(lex.add.entry '("hackers" nn (((hh a1) 1)((k ax r s) 0))))
(lex.add.entry '("hardware" nn (((hh a1 r d) 1)((u a r) 0))))
(lex.add.entry '("hdd" nn (((a1) 1)((ch e) 0)((D e1) 1)((D e1) 1))))
(lex.add.entry '("hlp" nn (((a1) 1)((ch e) 0)((e1) 1)((l e) 0)((p e1) 1))))
(lex.add.entry '("homepage" nn (((hh o u m) 0)((p e1 i zh) 1))))
(lex.add.entry '("home" nn (((hh o1 u m) 1))))
(lex.add.entry '("hotmail" nn (((hh o t) 0)((m e1 i l) 1))))
(lex.add.entry '("hp" nn (((a1) 1)((ch e) 0)((p e1) 1))))
(lex.add.entry '("html" nn (((a) 0)((ch e) 0)((t e) 0)((e)0)((m e) 0)((e1) 1)((l e) 0))))
(lex.add.entry '("http" nn (((a) 0)((ch e) 0)((t e1) 1)((t e) 0)((p e1) 1))))
(lex.add.entry '("hypertext" nn (((hh a i) 0)((p e r) 0)((t e1 k s t) 1))))
(lex.add.entry '("iagno" nn (((i a1) 1)((ny o) 0))))
(lex.add.entry '("icon" nn (((a1 i) 1)((k o n) 0))))
(lex.add.entry '("impress" nn (((i m) 0)((p r e1 s) 1))))
(lex.add.entry '("indisys" nn (((i n) 0)((d i) 0)((s i1 s) 1))))
(lex.add.entry '("inkscape" nn (((i n k) 0)((s k e1 i p) 1))))
(lex.add.entry '("intelligent" nn (((i n) 0)((t e1) 1)((l i) 0)((zh e n t) 0))))
(lex.add.entry '("ipbx" nn (((i1) 1)((p e) 0)((B e) 0)((e1) 1)((k i s) 0))))
(lex.add.entry '("ipc" nn (((i) 0)((p e1) 1)((th e1) 1))))
(lex.add.entry '("ip" nn (((i) 0)((p e1) 1))))
(lex.add.entry '("ipodder" nn (((a i) 0)((p o1 d) 1)((d ax r) 0))))
(lex.add.entry '("ipod" nn (((a1 i) 1)((p o d) 0))))
(lex.add.entry '("irc" nn (((i) 0)((e1 ) 1)((rr e) 0)((th e) 0))))
(lex.add.entry '("isbn" nn (((i) 0)((e1) 1)((s e) 0)((B e1) 1)((e1) 1)((n e) 0))))
(lex.add.entry '("isp" nn (((i1) 1)((e) 0)((s e)0)((p e) 0))))
(lex.add.entry '("istambul" nn (((i1 s) 1)((t a m) 0)((b u l) 0))))
(lex.add.entry '("itv" nn (((i) 0)((t e1) 1)((u1) 1)((B e) 0))))
(lex.add.entry '("jaws" nn (((zh o1 z) 1))))
(lex.add.entry '("jescs" nn (((x o1) 1)((t a) 0)((e1) 1)((e) 0)((s e) 0)((th e1) 1)((e) 0)((s e) 0))))
(lex.add.entry '("jfk" nn (((x o1) 1)((t a) 0)((e1) 1)((f e) 0)((k a1) 1))))
(lex.add.entry '("jokosher" nn (((ll o1 u) 1)((k o) 0)((sh e r) 0))))
(lex.add.entry '("jpeg" nn (((x o1) 1)((t a) 0)((p e) 0)((x e1) 1))))
(lex.add.entry '("jpg" nn (((x o1) 1)((t a) 0)((p e) 0)((x e1) 1))))
(lex.add.entry '("juicer" nn (((zh u1) 1)((s ax r) 0))))
(lex.add.entry '("k3b" nn (((k a) 1)((t r e s) 0)((b e1) 1))))
(lex.add.entry '("kbps" nn (((k i) 0)((l o) 0)((b a1 i t s) 1)((p o r) 0)((s e) 0)((G u1 n) 1)((d o) 0))))
(lex.add.entry '("kde" nn (((k a1) 1)((D e) 0)((e1) 1))))
(lex.add.entry '("kilobyte" nn (((k i) 0)((l o) 0)((B a1 i t) 1))))
(lex.add.entry '("kilobytes" nn (((k i) 0)((l o) 0)((B a1 i t s) 1))))
(lex.add.entry '("klotski" nn (((k l o1 t) 1)((s k i) 0))))
(lex.add.entry '("ko" nn (((k a1) 1)((o) 0))))
(lex.add.entry '("ksane" nn (((k a) 0)((s e1 i n) 1))))
(lex.add.entry '("kyle" nn (((k a1 i l) 1))))
(lex.add.entry '("layer" nn (((l e1 i) 1)((ax r) 0))))
(lex.add.entry '("lcd" nn (((e1) 1)((l e) 0)((th e1) 1)((D e1) 1))))
(lex.add.entry '("lemurae" nn (((l e) 0)((m u) 0)((rr a1) 1)((e) 0))))
(lex.add.entry '("lfp" nn (((e1) 1)((l e) 0)((e1) 1)((f e) 0)((p e1) 1))))
(lex.add.entry '("liferea" nn (((l a i f ) 0)((rr e1 a) 1))))
(lex.add.entry '("lnk" nn (((e1) 1)((l e) 0)((e1) 1)((n e) 0)((k a) 0))))
(lex.add.entry '("logjump" nn (((l o1 g) 1)((zh a m p) 0))))
(lex.add.entry '("lp" nn (((e1) 1)((l e) 0)((p e1) 1))))
(lex.add.entry '("lpc" nn (((e1) 1)((l e) 0)((p e1) 1)((th e1) 1))))
(lex.add.entry '("macromedia" nn (((m a) 0)((k r o) 0)((m e1) 1)((D i a) 0))))
(lex.add.entry '("mahjongg" nn (((m a1) 1)((i o n g) 0))))
(lex.add.entry '("mail" nn (((m e1 i l) 1))))
(lex.add.entry '("mails" nn (((m e1 i l s) 1))))
(lex.add.entry '("manager" nn (((m a) 1)((n a ) 0)((zh ax r) 0))))
(lex.add.entry '("mapquest" nn (((m a p) 0)((k u e1 s t) 1))))
(lex.add.entry '("mba" nn (((e1) 1)((m e) 0)((B e1) 1)((a1) 1))))
(lex.add.entry '("mbps" nn (((m e) 0)((G a) 0)((b a1 i t s) 1)((p o r) 0)((s e) 0)((G u1 n) 1)((d o) 0))))
(lex.add.entry '("megabyte" nn (((m e) 0)((G a) 0)((B a1 i t) 1))))
(lex.add.entry '("megabytes" nn (((m e) 0)((G a) 0)((B a1 i t s) 1))))
(lex.add.entry '("mellon" nn (((m e1) 1)((l o n) 0))))
(lex.add.entry '("menu" nn (((m e) 0) ((n u1) 1))))
(lex.add.entry '("messenger" nn (((m e1) 1)((s e n) 0)((ll ax r) 0))))
(lex.add.entry '("microsystem" nn (((m a i) 1)((k r o) 0)((s i1 s) 1)((t ax m) 0))))
(lex.add.entry '("mms" nn (((e1) 1)((m e) 0)((e1) 1)((m e) 0)((e1) 1)((s e) 0))))
(lex.add.entry '("mozilla" nn (((m o) 0)((z i1) 1)((l a) 0))))
(lex.add.entry '("mp3" nn (((e) 0)((m e) 0)((p e) 0)((t r e1 s) 1))))
(lex.add.entry '("mp3s" nn (((e) 0)((m e) 0)((p e) 0)((t r e1 s) 1))))
(lex.add.entry '("mpeg" nn (((e1) 1)((m e) 0)((p e) 0)((x e1) 1))))
(lex.add.entry '("mpg" nn (((e1) 1)((m e) 0)((p e) 0)((x e1) 1))))
(lex.add.entry '("mplayer" nn (((e1) 1)((m e) 0)((p l e1 i) 1)((ax r) 0))))
(lex.add.entry '("mp" nn (((e1) 1)((m e) 0)((p e1) 1))))
(lex.add.entry '("msn" nn (((e1) 1)((m e) 0)((e1) 1)((s e) 0)((e1) 1)((n e) 0))))
(lex.add.entry '("mtpaint" nn (((e1) 1)((m e) 0)((t e) 0)((p e1 i n t) 1))))
(lex.add.entry '("multisync" nn (((m u l) 0)((t i) 0)((s i1 n k) 1))))
(lex.add.entry '("myspace" nn (((m a i) 0)((s p e1 i s) 1))))
(lex.add.entry '("navigator" nn (((n a) 0)((v i) 0)((g e1 i) 1)((t ax r) 0))))
(lex.add.entry '("nba" nn (((e1) 1)((n e) 0)((B e1) 1)((a1) 1))))
(lex.add.entry '("nbc" nn (((e1) 1)((n e) 0)((B e1) 1)((th e1) 1))))
(lex.add.entry '("network" nn (((n e1 t) 1)((u o r k) 0))))
(lex.add.entry '("newline" nn (((n i u) 0)((l a1 i n) 1))))
(lex.add.entry '("nibbles" nn (((n i1) 1)((B ax l s) 0))))
(lex.add.entry '("ntfs" nn (((e1) 1)((n e) 0)((t e1) 1)((e1) 1)((f e) 0)((e1) 1)((s e) 0))))
(lex.add.entry '("ntsc" nn (((e1) 1)((n e) 0)((t e1) 1)((e) 0)((s e) 0)((th e1) 1))))
(lex.add.entry '("nvidia" nn (((e n) 0)((b i1) 1)((D i a) 0))))
(lex.add.entry '("nvu" nn (((e1) 1)((n e) 0)((u1) 1)((B e) 0)((u1) 1))))
(lex.add.entry '("office" nn (((o1) 1)((f i s) 0))))
(lex.add.entry '("ogg" nn (((o1) 1)((x e) 0)((x e) 0))))
(lex.add.entry '("oit" nn (((o1) 1)((i1) 1)((t e1) 1))))
(lex.add.entry '("ok" nn (((o u) 0)((k e1 i) 1))))
(lex.add.entry '("olp" nn (((o1) 1)((e1) 1)((l e) 0)((p e1) 1))))
(lex.add.entry '("omc" nn (((o1) 1)((e1) 1)((m e) 0)((th e1) 1))))
(lex.add.entry '("oms" nn (((o1) 1)((e1) 1)((m e) 0)((e1) 1)((s e) 0))))
(lex.add.entry '("ong" nn (((o1) 1)((e1) 1)((n e) 0)((x e1) 1))))
(lex.add.entry '("open" nn (((o1) 1)((p e n) 0))))
(lex.add.entry '("openoffice" nn (((o1) 1)((p e n) 0)((o1) 1)((f i s) 0))))
(lex.add.entry '("openwengo" nn (((o1) 1)((p e n) 0)((u e1 n) 1)((g o u) 0))))
(lex.add.entry '("ops" nn (((o1) 1)((p e1) 1)((e1) 1)((s e) 0))))
(lex.add.entry '("outlook" nn (((a1 u t) 1)((l u k) 0))))
(lex.add.entry '("p2p" nn (((p e) 0)((D o1 s) 1)((p e) 0))))
(lex.add.entry '("package" nn (((p a1) 1)((k i zh) 0))))
(lex.add.entry '("page" nn (((p e1 i zh) 1))))
(lex.add.entry '("pagerank" nn (((p e1 i zh) 1)((rr a n k) 0))))
(lex.add.entry '("pages" nn (((p e1 i) 1)((zh i s) 0))))
(lex.add.entry '("paint" nn (((p a1 i n t)1))))
(lex.add.entry '("pbx" nn (((p e) 0)((B e1) 1)((e) 0)((k i s) 0))))
(lex.add.entry '("pcmcia" nn (((p e1) 1)((th e) 0)((m e) 0)((th i1) 1)((a) 0))))
(lex.add.entry '("pc" nn (((p e) 0)((th e1) 1))))
(lex.add.entry '("pda" nn (((p e) 0)((D e1) 1)((a) 0))))
(lex.add.entry '("pdf" nn (((p e) 0)((D e) 0)((e1) 1)((f e) 0))))
(lex.add.entry '("pluggin" nn (((p l a1 g) 1)((i n) 0))))
(lex.add.entry '("plugin" nn (((p l a1 g) 1)((i n) 0))))
(lex.add.entry '("plug" nn (((p l a1 g) 1))))
(lex.add.entry '("pm" nn (((p e1) 1)((e1) 1)((m e) 0))))
(lex.add.entry '("png" nn (((p e1) 1)((e) 0)((n e) 0)((x e1) 1))))
(lex.add.entry '("podcasting" nn (((p o d) 0)((k a1 s) 1)((t i n g) 0))))
(lex.add.entry '("pop3" nn (((p o p) 0)((t r e1 s) 1))))
(lex.add.entry '("postscript" nn (((p o1 s t) 1)((s k r i p t) 0))))
(lex.add.entry '("powerpoint" nn (((p a1) 1) ((u e r) 0) ((p o1 i n t) 1))))
(lex.add.entry '("pp" nn (((p e) 1)((p e1) 1))))
(lex.add.entry '("ppt" nn (((p e1) 1)((p e1) 1)((t e1) 1))))
(lex.add.entry '("psoe" nn (((p e) 0)((s o1) 1)((e) 0))))
(lex.add.entry '("pvc" nn (((p e1) 1)((u1) 1)((B e) 0)((th e1) 1))))
(lex.add.entry '("pvp" nn (((p e1) 1)((u1) 1)((B e) 0)((p e1) 1))))
(lex.add.entry '("pytv" nn (((p i) 0)((t e) 0)((u1) 1)((B e) 0))))
(lex.add.entry '("qcad" nn (((k u) 0)((k a1 d) 1))))
(lex.add.entry '("quicken" nn (((k u1 i) 1)((k ax n) 0))))
(lex.add.entry '("quicklaunch" nn (((k u i k) 0)((l o1 n ch) 1))))
(lex.add.entry '("quick" nn (((k u i1 k) 1))))
(lex.add.entry '("rar" nn (((rr a1 r) 1))))
(lex.add.entry '("rdf" nn (((e) 1)((rr e) 0)((D e) 0)((e1) 1)((f e) 0))))
(lex.add.entry '("rdsi" nn (((e1) 1)((rr e) 0)((D e1) 1)((e1) 1)((s e) 0)((i1) 1))))
(lex.add.entry '("reader" nn (((rr i1) 1)((D ax r) 0))))
(lex.add.entry '("recorder" nn (((r i) 0)((k o1 r) 1)((d ax r) 0))))
(lex.add.entry '("rh" nn (((e1) 1)((rr e) 0)((a1) 1)((ch e) 0))))
(lex.add.entry '("rhythmbox" nn (((r i1) 1)((dh o m) 0)((b o1 k s) 1))))
(lex.add.entry '("rip" nn (((r i1 p) 1))))
(lex.add.entry '("rne" nn (((e1) 1)((rr e) 0)((e1) 1)((n e) 0)((e1) 1))))
(lex.add.entry '("roller" nn (((rr o1) 1)((l ax r) 0))))
(lex.add.entry '("rom" nn (((rr o1 m) 1))))
(lex.add.entry '("root" nn (((rr u1 t) 1))))
(lex.add.entry '("router" nn (((rr u1) 1)((t ax r) 0))))
(lex.add.entry '("routers" nn (((rr u1) 1)((t ax r s) 0))))
(lex.add.entry '("rpm" nn (((e1) 1)((rr e) 0)((p e1) 1)((e) 0)((m e) 0))))
(lex.add.entry '("rss" nn (((e1) 1)((rr e) 0)((e1) 1)((s e) 0)((e1) 1)((s e) 0))))
(lex.add.entry '("rtve" nn (((e1) 1)((rr e) 0)((t e1) 1)((u1) 1)((B e) 0)((e1) 1))))
(lex.add.entry '("rw" nn (((e1) 1)((rr e) 0)((u1) 1)((B e) 0)((D o1) 1)((b l e) 0))))
(lex.add.entry '("same" nn (((s e1 i m)1))))
(lex.add.entry '("sawfish" nn (((s o1) 1)((f i sh) 0))))
(lex.add.entry '("saytext" nn (((s e1 i) 1)((t e k s t) 0))))
(lex.add.entry '("scim" nn (((e) 1) ((s e) 0)((th e1) 1)((i1) 1)((e1) 1)((m e) 0))))
(lex.add.entry '("scm" nn (((e) 1) ((s e) 0)((th e1) 1)((e1) 1)((m e) 0))))
(lex.add.entry '("selected" nn (((s e) 0)((l e1 k) 1)((t i d) 0))))
(lex.add.entry '("sendto" nn (((s e1 n d) 1)((t u) 0))))
(lex.add.entry '("serpentine" nn (((s e r) 0)((p ax n) 0)((t a1 i n) 1))))
(lex.add.entry '("server" nn (((s e1 r) 1)((v ax r) 0))))
(lex.add.entry '("servlet" nn (((s e1 r v) 1)((l e t) 0))))
(lex.add.entry '("sgae" nn (((e1) 1)((s e) 0)((x e1) 1)((a1) 1)((e1) 1))))
(lex.add.entry '("sgml" nn (((e) 1)((s e) 0)((x e) 0)((e1) 0)((m e) 0)((e1) 1)((l e) 0))))
(lex.add.entry '("shareware" nn (((sh e1 a r) 1)((u a r) 0))))
(lex.add.entry '("shift" nn (((sh i1 f t) 1))))
(lex.add.entry '("shopping" nn (((sh o1) 1)((p i n g) 0))))
(lex.add.entry '("size" nn (((s a1 i s) 1))))
(lex.add.entry '("slash" nn (((s l a1 sh) 1))))
(lex.add.entry '("sl" nn (((e1) 1)((s e) 0)((e1) 1)((l e) 0))))
(lex.add.entry '("sms" nn (((e1) 1)((s e) 0)((e1) 1)((m e) 0)((e1) 1)((s e) 0))))
(lex.add.entry '("smtp" nn (((e) 1)((s e) 0)((e1) 0)((m e) 0)((t e) 0)((p e1) 1))))
(lex.add.entry '("softphone" nn (((s o1 f t) 1) ((f o u n) 0))))
(lex.add.entry '("software" nn (((s o1 f t) 1)((u a r) 0))))
(lex.add.entry '("sound" nn (((s a1 u n d) 1))))
(lex.add.entry '("soundtracker" nn (((s a u n d) 0)((t r a1) 1)((k ax r) 0))))
(lex.add.entry '("source" nn (((s o1 r s) 1))))
(lex.add.entry '("space" nn (((s p e1 i s) 1))))
(lex.add.entry '("spammer" nn (((s p a1) 1)((m ax r) 0))))
(lex.add.entry '("spammers" nn (((s p a1) 1)((m ax r s) 0))))
(lex.add.entry '("ssl" nn (((e) 0)((s e) 0)((e) 0)((s e) 0)((e1) 1)((l e) 0))))
(lex.add.entry '("staroffice" nn (((s t a r) 0)((o1) 1)((f i s) 0))))
(lex.add.entry '("stock" nn (((s t o1 k) 1))))
(lex.add.entry '("streaming" nn (((s t r i1) 1)((m i n g) 0))))
(lex.add.entry '("streamtuner" nn (((s t r i m) 0)((t i1 u) 1)((n ax r) 0))))
(lex.add.entry '("sun" nn (((s a1 n) 1))))
(lex.add.entry '("synaptic" nn (((s i) 0)((n a1 p) 1)((t i k) 0))))
(lex.add.entry '("system" nn (((s i1 s) 1)((t ax m) 0))))
(lex.add.entry '("systems" nn (((s i1 s) 1)((t e m s) 0))))
(lex.add.entry '("tcp" nn (((t e) 0)((th e) 0)((p e1) 1))))
(lex.add.entry '("tdt" nn (((t e1) 1)((D e) 0)((t e1) 1))))
(lex.add.entry '("terabyte" nn (((t e) 0)((r a) 0)((B a1 i t) 1))))
(lex.add.entry '("terabytes" nn (((t e) 0)((r a) 0)((B a1 i t s) 1))))
(lex.add.entry '("texmax" nn (((t e1 k s) 1)((m ax k s) 0))))
(lex.add.entry '("tft" nn (((t e1) 1)((e1) 1)((f e) 0)((t e1) 1))))
(lex.add.entry '("tgz" nn (((t e1) 1)((x e1) 1)((th e1) 1)((t a) 0))))
(lex.add.entry '("the" nn (((th e) 0))))
(lex.add.entry '("thunderbird" nn (((th a1 n) 1)((d e r) 0)((b e r d) 0))))
(lex.add.entry '("tld" nn (((t e) 0) ((e1) 1) ((l e) 0) ((D e1) 1))))
(lex.add.entry '("tmp" nn (((t e1) 1)((e1) 1)((m e) 0)((p e1) 1))))
(lex.add.entry '("tnt" nn (((t e1) 1)((e1) 1)((n e) 0)((t e1) 1))))
(lex.add.entry '("tome" nn (((t o1 u m) 1))))
(lex.add.entry '("totem" nn (((t o1) 1)((t e m) 0))))
(lex.add.entry '("tts" nn (((t e1) 1)((t e1) 1)((e1) 1)((s e) 0))))
(lex.add.entry '("tuxpaint" nn (((t a k s) 1)((p e1 i n t) 0))))
(lex.add.entry '("tve" nn (((t e1) 1)((u1) 1)((B e) 0)((e1) 1))))
(lex.add.entry '("tv" nn (((t e1) 1)((u1) 1)((B e) 0))))
(lex.add.entry '("tvtime" nn (((t e) 0)((u1) 1)((B e) 0)((t a i m) 1))))
(lex.add.entry '("txt" nn (((t e) 0)((e1) 1)((k i s) 0)((t e1) 1))))
(lex.add.entry '("ue" nn (((u) 0)((n i o1 n) 1)((e u) 0)((r o) 0)((p e1) 1)((a) 0))))
(lex.add.entry '("ugt" nn (((u1) 1)((x e1) 1)((t e1) 1))))
(lex.add.entry '("uk" nn (((u1) 1)((k a1) 1))))
(lex.add.entry '("university" nn (((i u) 0)((n i) 0)((v e1 r) 1)((s i) 0)((t i) 0))))
(lex.add.entry '("unix" nn (((i u1) 1)((n i k s) 0))))
(lex.add.entry '("unselected" nn (((a n) 0)((s e) 0)((l e1 k) 1)((t i d) 0))))
(lex.add.entry '("up" nn (((a p) 0))))
(lex.add.entry '("url" nn (((u1) 1)((e) 0)((rr e) 0)((e1) 1)((l e) 0))))
(lex.add.entry '("urn" nn (((u) 0) ((e1) 1) ((rr e) 0) ((e1) 1) ((n e) 0))))
(lex.add.entry '("usb" nn (((u1) 1)((e1) 1)((s e) 0)((B e1) 1))))
(lex.add.entry '("vanity" nn (((v a1) 1)((n i) 0)((t i) 0))))
(lex.add.entry '("vfs" nn (((u1) 1)((B e) 0)((e1) 1)((f e) 0)((e1) 1)((s e) 0))))
(lex.add.entry '("vhs" nn (((u1) 1)((B e) 0)((a1) 1)((ch e) 0)((e1) 1)((s e) 0))))
(lex.add.entry '("vih" nn (((u1) 1)((B e) 0)((i1) 1)((a1) 1)((ch e) 0))))
(lex.add.entry '("voip" nn (((b o1) 1)((i) 0)((p e1) 1))))
(lex.add.entry '("vpo" nn (((u1) 1)((B e) 0)((p e1) 1)((o1) 1))))
(lex.add.entry '("vrml" nn (((u1) 1) ((B e) 0) ((e1) 1) ((rr e) 0) ((e1) 1) ((m e) 0) ((e1) 1) ((l e) 0))))
(lex.add.entry '("w3c" nn (((u1) 1)((B e) 0)((D o1) 1)((b l e) 0)((t r e1 s) 1)((th e1) 1))))
(lex.add.entry '("watermain" nn (((u o1) 1)((t ax r) 0)((m e1 i n) 1))))
(lex.add.entry '("wave" nn (((u e1 i v) 1))))
(lex.add.entry '("wavesurfer" nn (((u e1 i f) 1) ((s a r) 0)((f ax r) 0))))
(lex.add.entry '("wav" nn (((u a1 f) 1))))
(lex.add.entry '("webcam" nn (((u e b) 0)((k a1 m) 1))))
(lex.add.entry '("webmail" nn (((u e b) 0)((m e1 i l) 1))))
(lex.add.entry '("webmaster" nn (((u e b) 0)((m a1 s) 1)((t e r) 0))))
(lex.add.entry '("web" nn (((u e1 b) 1))))
(lex.add.entry '("westnorth" nn (((u e1 s t) 1)((n o r th) 0))))
(lex.add.entry '("window" nn (((u i1 n) 1)((d o u) 0))))
(lex.add.entry '("windows" nn (((u i1 n) 1)((d o u s) 0))))
(lex.add.entry '("wings" nn (((u i1 n g s) 1))))
(lex.add.entry '("wireless" nn (((u a1 i r) 1)((l e s) 0))))
(lex.add.entry '("wish" nn (((u i1 sh) 1))))
(lex.add.entry '("wlan" nn (((u) 0) ((B e) 0) ((d o1) 1) ((b l e) 0)((l a1 n) 1))))
(lex.add.entry '("workspace" nn (((u o1 r k) 1)((s p e i s) 0))))
(lex.add.entry '("writer" nn (((rr a1 i) 1)((t ax r) 0))))
(lex.add.entry '("www" nn (((u1) 1) ((B e) 0) ((d o1) 1) ((b l e) 0) ((u1) 1) ((B e) 0) ((d o1) 1) ((b l e) 0) ((u1) 1) ((B e) 0) ((d o1) 1) ((b l e) 0))))
(lex.add.entry '("xcf" nn (((e1) 1)((k i s) 0)((th e1) 1)((e)0)(( f e)0))))
(lex.add.entry '("xchat" nn (((e1) 1)((k i s) 0)((ch a1 t) 1))))
(lex.add.entry '("xhtml" nn (((e1) 1)((k i s) 0)((a) 0)((ch e) 0)((t e) 0)((e)0)((m e) 0)((e1) 1)((l e) 0))))
(lex.add.entry '("xls" nn (((e1) 1)((k i s) 0)((e1) 1)((l e) 0)((e1) 1)((s e) 0))))
(lex.add.entry '("xml" nn (((e1) 1)((k i s) 0)((e1) 1)((m e) 0)((e1) 1)((l e) 0))))
(lex.add.entry '("xmms" nn (((e1) 1)((k i s) 0)((e1) 1)((m e) 0)((e) 0)((m e) 0)((e1) 1)((s e) 0))))
(lex.add.entry '("xpdf" nn (((e1) 1)((k i s) 0)((p e) 0)((D e) 0)((e1) 1)((f e) 0))))
(lex.add.entry '("xp" nn (((e)0 )((k i s) 0)((p e1) 1))))
(lex.add.entry '("xsane" nn (((e1) 1)((k i s) 0)((s e1 i n) 1))))
(lex.add.entry '("xsl" nn (((e1) 1) ((k i s) 0) ((e1) 1) ((s e) 0) ((e1) 1) ((l e) 0))))
(lex.add.entry '("xzoom" nn (((e1) 1)((k i s) 0)((th u1 m) 1))))
(lex.add.entry '("yahoo" nn (((ll a) 0)((hh u1) 1))))
(lex.add.entry '("youtube" nn (((ll u) 0)((t i u1 b) 1))))
(lex.add.entry '("zoom" nn (((th u1 m) 1))))
;(lex.add.entry '("jon" nn (((zh o1 n) 1))))
;(lex.add.entry '("josep" nn (((ll o) 0)((s e1 p) 1))))
;(lex.add.entry '("iratxe" nn (((i) 0)((r a1) 1)((ch e) 0))))
;(lex.add.entry '("valls" nn (((v a1 l s) 1))))
;(lex.add.entry '("blanch" nn (((b l a1 n t s)1))))
;(lex.add.entry '("bachmayer" nn (((b a t s)0)((m a1)1)((ll e r)0))))
;(lex.add.entry '("carroll" nn (((k a1) 1)((rr o l) 0))))
;(lex.add.entry '("washington" nn (((u a1)1)((sh i n g)0)((t o n)0))))


   ;;;Letters
;(lex.add.entry '("a" nn (((a) 0))))
(lex.add.entry '("b" nn (((b e) 0))))
(lex.add.entry '("c" nn (((th e) 0))))
(lex.add.entry '("ch" nn (((ch e) 0))))
(lex.add.entry '("d" nn (((d e) 0))))
;(lex.add.entry '("e" nn (((e) 0))))
(lex.add.entry '("f" nn (((e1) 1)((f e) 0))))
(lex.add.entry '("g" nn (((x e) 0))))
(lex.add.entry '("h" nn (((a1) 1)((ch e) 0))))
;(lex.add.entry '("i" nn (((i) 0))))
(lex.add.entry '("j" nn (((x o1) 1)((t a) 0))))
(lex.add.entry '("k" nn (((k a) 0))))
(lex.add.entry '("l" nn (((e1) 1)((l e) 0))))
(lex.add.entry '("m" nn (((e1) 1)((m e) 0))))
(lex.add.entry '("n" nn (((e1) 1)((n e) 0))))
(lex.add.entry '("~n" nn (((e1) 1)((ny e) 0))))
(lex.add.entry '("ñ" nn (((e1) 1)((ny e) 0))))
;(lex.add.entry '("o" nn (((o) 0))))
(lex.add.entry '("p" nn (((p e) 0))))
(lex.add.entry '("q" nn (((k u) 0))))
(lex.add.entry '("r" nn (((e1) 1)((rr e) 0))))
(lex.add.entry '("s" nn (((e1) 1) ((s e) 0))))
(lex.add.entry '("t" nn (((t e) 0))))
;(lex.add.entry '("u" nn (((u) 0))))
(lex.add.entry '("v" nn (((u1) 1)((B e) 0))))
(lex.add.entry '("w" nn (((u) 0) ((B e) 0) ((D o1) 1) ((b l e) 0))))
(lex.add.entry '("x" nn (((e1) 1)((k i s) 0))))
;(lex.add.entry '("y" nn (((i) 0)((g r i e1) 1))((G a) 0)))   ;; doubt
(lex.add.entry '("z" nn (((th e1) 1)((t a) 0))))
;(lex.add.entry '("á" nn (((a) 0))))
;(lex.add.entry '("é" nn (((e) 0))))
;(lex.add.entry '("í" nn (((i) 0))))
;(lex.add.entry '("ó" nn (((o) 0))))
;(lex.add.entry '("ú" nn (((u) 0))))
;(lex.add.entry '("ü" nn (((u) 0))))


   ;;;Symbols
(lex.add.entry 
 '("*" n (((a s) 0) ((t e) 0) ((r i1 s) 1)  ((k o) 0))))
(lex.add.entry 
 '("%" n (((p o r) 0) ((th i e1 n) 1) ((t o) 0))))
(lex.add.entry 
 '("&" n (((a1 m) 1) ((p e r) 0) ((s a n) 0))))
(lex.add.entry 
 '("$" n (((d o1) 1) ((l a r) 0))))
(lex.add.entry 
 '("¤" n (((e1 u) 1) ((r o) 0))))
(lex.add.entry 
 '("#" n (((a l) 0) ((m o a) 0) ((D i1) 1) ((ll a) 0))))
(lex.add.entry 
 '("@" n (((a) 0) ((rr o1) 1) ((B a) 0))))
(lex.add.entry 
 '("+" n (((m a s) 0)) ((pos "K7%" "OA%" "T-%"))))
;(lex.add.entry 
; '("^" n (((a) 0) ((th e1 n) 1))((t o) 0)((th i r) 0)((k u n) 0)((f l e1) 1)((x o) 0)))
(lex.add.entry 
 '("^" n (((k a1) 1) ((r e t) 0)) ((pos "K6$"))))
(lex.add.entry 
 '("~" n (((t i1 l) 1) ((d e) 0)) ((pos "K6$"))))
(lex.add.entry 
 '("=" n (((i) 0) ((G u a1 l) 1))))
(lex.add.entry 
 '("/" n (((b a1) 1) ((rr a) 0))))  ;; $$$division, etc.
;(lex.add.entry 
; '("/" n (((e1 n ) 1) ((t r e) 0))))  ;; $$$division, etc.
(lex.add.entry 
 '("\\" n (((b a1) 1) ((rr a) 1) ((i n) 0) ((b e r) 0) ((t i1) 1) ((D a) 0))))
(lex.add.entry 
 '("_" n (((g i) 0) ((o1 n) 1) ((b a1) 1) ((x o) 0)) ))
(lex.add.entry 
 '("|" n (((b a1) 1) ((rr a) 0))))
(lex.add.entry 
 '(">" n ((( m a ) 0) ((ll o1 r) 1) ((k e) 0))))
(lex.add.entry 
 '("<" n ((( m e ) 0) ((n o1 r) 1) ((k e) 0))))
(lex.add.entry 
 '("(" n ((( a) 0) ((b r i1 r) 1) ((p a) 0)((r e1 n) 1)((t e) 0)((s i s) 0))))
(lex.add.entry 
 '(")" n (((th e) 0) ((rr a1 r) 1) ((p a) 0)((r e1 n) 1)((t e) 0)((s i s) 0))))
(lex.add.entry 
 '("[" n ((( a) 0) ((b r i1 r) 1) ((k o r) 0)((ch e1) 1)((t e) 0))))
(lex.add.entry 
 '("]" n (((th e) 0) ((rr a1 r) 1) ((k o r) 0)((ch e1) 1)((t e) 0))))
(lex.add.entry 
 '("{" n ((( a) 0) ((b r i1 r) 1)((ll a1) 1)((B e) 0))))
(lex.add.entry 
 '("}" n (((th e) 0) ((rr a1 r) 1)((ll a1) 1)((B e) 0))))
(lex.add.entry 
 '(" " n (((e s) 0)((p a1) 1)((th i o) 0))))
(lex.add.entry 
 '("\t" n (((t a1 b) 1))))
(lex.add.entry 
 '("\n" n (((n u e1) 1) ((B a) 0) ((l i1) 1) ((n e) 0) (( a) 0))))

   ;;;Punctuation
(lex.add.entry '("." punc nil))
(lex.add.entry '("." nn (((p u1 n) 1) ((t o) 0))))
(lex.add.entry '("'" punc nil))
(lex.add.entry '("'" nn (((k o) 0) ((m i1) 1) ((ll a) 0) ((s i1 m) 1) ((p l e) 0))))
(lex.add.entry '(":" punc nil))
(lex.add.entry '(":" nn (((d o s) 0)((p u1 n) 1) ((t o s) 0))))
(lex.add.entry '(";" punc nil))
(lex.add.entry '(";" nn (((p u1 n) 1) ((t o) 0) ((i) 0) ((k o1) 1) ((m a) 0))))
(lex.add.entry '("," punc nil))
;(lex.add.entry '("," nn (((k o1) 1) ((m a) 0))))
(lex.add.entry '("-" punc nil))
;(lex.add.entry '("-" nn (((g i) 0) ((o1 n) 1) ((m e1) 1) ((D i o) 0))))
(lex.add.entry '("\"" punc nil))
(lex.add.entry '("\"" nn (((k o) 0) ((m i1) 1) ((ll a) 0) ((D o1) 1) ((b l e) 0))))
(lex.add.entry '("`" punc nil))
(lex.add.entry '("`" nn (((a) 0) ((th e1 n) 1) ((t o) 0))))
(lex.add.entry '("?" punc nil))
(lex.add.entry '("?" nn (((th e) 0) ((rr a1 r) 1) ((i n) 0)((t e) 0)((rr o) 0)((G a) 0)((th i o1 n) 1))))
(lex.add.entry '("!" punc nil))
(lex.add.entry '("!" nn (((th e) 0) ((rr a1 r) 1) ((e k s) 0)((k l a) 0)((m a) 0)((th i o1 n) 1))))
(lex.add.entry '("¿" punc nil))
(lex.add.entry '("¿" nn (((a) 0) ((b r i1 r) 1) ((i n) 0)((t e) 0)((rr o) 0)((G a) 0)((th i o1 n) 1))))
(lex.add.entry '("¡" punc nil))
(lex.add.entry '("¡" nn (((a) 0) ((b r i1 r) 1) ((e k s) 0)((k l a) 0)((m a) 0)((th i o1 n) 1))))
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Hand written letter to sound rules
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;  The following function is called when a given word is not found in the lexicon/addenda

(define (Indisys_MP_es_lts word features)
  "(Indisys_MP_es_lts WORD FEATURES)
Using various letter to sound rules build a Spanish pronunciation of 
WORD."
;     (format t "\nBeginning of LTS: %l" word)
  (let (phones syl stresssyl dword weakened)
;     (format t "\nValue before checking against alphabet: %l %l" word features)
    (if (lts.in.alphabet word 'downcase_allcharacters)
	(set! dword (lts.apply word 'downcase_allcharacters))
	(set! dword (lts.apply 'uups 'downcase_allcharacters)))
;     (format t "\nBefore LTS, to list spanish: %l" dword)
    (set! phones (lts.apply dword 'follow_spanish_phonetic_rules))
;     (format t "\nEntry to spanish_syl: %l" phones)
    (set! syl (lts.apply phones 'break_words_into_syl))
;     (format t "\nOutput from spanish_syl: %l" syl)
    (if (spanish_is_a_content_word 
	 (apply string-append dword)
	 spanish_guess_pos)
	(set! stresssyl (lts.apply syl 'assign_stress_tovowels))
	(set! stresssyl syl))  ;; function words leave as is
;     (format t "\nContent of stressyl: %l" stresssyl)
    (list word
	  nil
	  (spanish_tosyl_brackets stresssyl))))

(define (spanish_is_a_content_word word poslist)
  "(spanish_is_a_content_word WORD POSLIST)
Check explicit list of function words and return t if this is not
listed."
  (cond
   ((null poslist)
    t)
   ((member_string word (cdr (car poslist)))
    nil)
   (t
    (spanish_is_a_content_word word (cdr poslist)))))


(define (spanish_tosyl_brackets phones)
   "(spanish_tosyl_brackets phones)
Takes a list of phones containing - as syllable boundary.  Construct the
Festival bracket structure."
 (let ((syl nil) (syls nil) (p phones) (stress 0))
    (while p
     (set! syl nil)
     (set! stress 0)
     (while (and p (not (eq? '- (car p))))
       (set! syl (cons (car p) syl))
       (if (string-matches (car p) ".*1")
           (set! stress 1))
       (set! p (cdr p)))
     (set! p (cdr p))  ;; skip the syllable separator
     (set! syls (cons (list (reverse syl) stress) syls)))
    (reverse syls)))   


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Downcase and accents
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; This set of rules downcases input characters
;;; It applies before the set of rules "follow_spanish_phonetic_rules"

(lts.ruleset
 downcase_allcharacters
 ( )
 (
  ( [ a ] = a )
  ( [ e ] = e )
  ( [ i ] = i )
  ( [ o ] = o )
  ( [ u ] = u )
  ( [ á ] = á )
  ( [ é ] = é )
  ( [ í ] = í )
  ( [ ó ] = ó )
  ( [ ú ] = ú )
  ( [ ü ] = ü ) 
  ( [ b ] = b )
  ( [ c ] = c )
  ( [ "ç" ] = s )
  ( [ d ] = d )
  ( [ f ] = f )
  ( [ g ] = g )
  ( [ h ] = h )
  ( [ j ] = j )
  ( [ k ] = k )
  ( [ l ] = l )
  ( [ m ] = m )
  ( [ n ] = n )
  ( [ ñ ] =  ñ )
  ( [ p ] = p )
  ( [ q ] = q )
  ( [ r ] = r )
  ( [ s ] = s )
  ( [ t ] = t )
  ( [ v ] = v )
  ( [ w ] = w )
  ( [ x ] = x )
  ( [ y ] = y )
  ( [ z ] = z )
  ( [ "\'" ] = "\'" )
  ( [ : ] = : )
  ( [ ~ ] = ~ )
  ( [ "\"" ] = "\"" )
  ( [ A ] = a )
  ( [ E ] = e )
  ( [ I ] = i )
  ( [ O ] = o )
  ( [ U ] = u )
  ( [ Á ] = á )
  ( [ É ] = é )
  ( [ Í ] = í )
  ( [ Ó ] = ó )
  ( [ Ú ] = ú )
  ( [ Ü ] = ü ) 
  ( [ B ] = b )
  ( [ C ] = c )
  ( [ "Ç" ] = s )
  ( [ D ] = d )
  ( [ F ] = f )
  ( [ G ] = g )
  ( [ H ] = h )
  ( [ J ] = j )
  ( [ K ] = k )
  ( [ L ] = l )
  ( [ M ] = m )
  ( [ N ] = n )
  ( [ Ñ ] =  ñ )
  ( [ P ] = p )
  ( [ Q ] = q )
  ( [ R ] = r )
  ( [ S ] = s )
  ( [ T ] = t )
  ( [ V ] = v )
  ( [ W ] = w )
  ( [ X ] = x )
  ( [ Y ] = y )
  ( [ Z ] = z )
))

;;; This other set of rules apply Spanish phonetic rules to the given input.
;;; It is called after "downcase_allcharacters"

(lts.ruleset
 follow_spanish_phonetic_rules
 (
  (LNS l n s )
  (DNSR d n s r )
  (EI e i é í)  ; note that accented vowels are included in this set
  (AEIOUt  á é í ó ú )
  (V a e i o u )
  (C b B c d D f g G h j k l m n ñ ~ p q r s t v w x y z )
 )
  ;;; Spanish phonetic LTS rules 
(
 ; Break diphthongs at the end of words (atribuid, atribuido)
 ( "'" V* C* u [ i ] DNSR # = i ) 
 ( AEIOUt V* C* u [ i ] DNSR # = i )   
 ( u [ i ] DNSR # = i1 ) 
 ( "'" V* C* u [ i ] d V # = i ) 
 ( AEIOUt V* C* u [ i ] d V # = i ) 
 ( u [ i ] d AEIOUt # = i )   
 ( u [ i ] d V # = i1 )

 ( [ a ] = a )
 ( [ e ] = e )
 ( [ i ] = i )
 ( [ o ] = o )
 ( [ u ] = u )
 ( [ "'" a ] = a1 )
 ( [ "'" e ] = e1 )
 ( [ "'" i ] = i1 )
 ( [ "'" o ] = o1 )
 ( [ "'" u ] = u1 )
 ( [ á ] = a1 )
 ( [ é ] = e1 )
 ( [ í ] = i1 )
 ( [ ó ] = o1 )
 ( [ ú ] = u1 )
 ( [ ":" u ] = u )      ; umlaut (it should not get this far; but just in case...)
 ( [ "\"" u ] = u )  
 ( [ ü ] = u ) 

 ( V [ b ] V = B ) ; -- added
 ( AEIOUt [ b ] AEIOUt = B ) ; -- added
 ( AEIOUt [ b ] V = B ) ; -- added
 ( V [ b ] AEIOUt = B ) ; -- added
 ( [ b ] = b )

 ( V [ v ] V = B) ; -- added
 ( AEIOUt [ v ] AEIOUt = B ) ; -- added
 ( AEIOUt [ v ] V = B ) ; -- added
 ( V [ v ] AEIOUt = B ) ; -- added
 ( [ v ] = b )

 ( [ c ] "'" EI = th )
 ( [ c ] EI = th )
 ( [ c h ] = ch )
 ( [ c ] = k )

 ( V [ d ] V = D) ; -- added
 ( AEIOUt [ d ] AEIOUt = D ) ; -- added
 ( AEIOUt [ d ] V = D ) ; -- added
 ( V [ d ] AEIOUt = D ) ; -- added
 ( [ d ] = d )

 ( [ f ] = f )
 ( [ g ] "'" EI = x )
 ( [ g ] EI = x )
 ( [ g u ] "'" EI = g )
 ( [ g u ] EI = g )

 ( [ g ":" u ] EI = g u )      ; umlaut
 ( [ g ":" u ] "'" EI = g u ) 
 ( [ g "\"" u ] EI = g u )  
 ( [ g "\"" u ] "'" EI = g u ) 
 ( [ g ü ] EI = g u ) 
 ( [ g ü ] "'" EI = g u ) 

 ( V [ g ] V = G ) ; -- added
 ( AEIOUt [ g ] AEIOUt = G ) ; -- added
 ( AEIOUt [ g ] V = G ) ; -- added
 ( V [ g ] AEIOUt = G ) ; -- added
 ( [ g ] = g )

 ( [ h ] =  )
 ( [ j ] = x )
 ( [ k ] = k )
 ( [ l l ] # = l )
 ( [ l l ] = ll )
 ( [ l ] = l )
 ( [ m ] = m )
 ( [ "~" n ] = ny )
 ( [ ñ ] = ny )
 ( [ n ] = n )
 ( [ p ] = p )
 ( [ p h ] = f )  ; like in Greek
 ( [ q u ] a = k u )  ; for words like aquarium, quo, etc.
 ( [ q u ] = k )
 ( [ q ] = k ) ; this should not happen, but if one might type it...
 ( [ r r ] = rr )
 ( # [ r ] = rr )
 ( LNS [ r ] = rr )
 ( [ r ] = r )
 ( [ s ] = s )
 ( # [ s ] C = e s ) ;this will not apply because the rule ([s] = s) precedes (# [s] C = es)
 ( # [ s ] "'" C = e s ) ;same as above
 ( # [ s ] ":" C = e s ) ;same as above
 ( # [ s ] "\"" C = e s ) ;same as above
 ( [ t ] = t )
 ( [ w ] = u )
 ( [ x ] = k s )

 ( [ y ] # = i )
 ( [ y ] C = i )
 ( [ y ] "'" C = i )
 ( [ y ] ":" C = i )
 ( [ y ] "\"" C = i )
 ( [ y ] = ll )

 ( [ z ] = th )

  ; quotes are used for vowel accents in foreign keyboards (i.e. cami'on).
  ; remove those that were not before a vowel. same with other signs.
 ( [ "'" ] = )  
 ( [ ":" ] = )  
 ( [ "\"" ] = )  
 ( [ "~" ] = )  
  ))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Spanish syllabification by rewrite rules
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; These rules break words into syllables (i. e. Indisys --> (i n - d i - s i1 s))
;; It is called after "follow_spanish_phonetic_rules"

(lts.ruleset
   break_words_into_syl
   (  (V a1 i1 u1 e1 o1 a i u e o )
      (IUT i1 u1 )
      (C b B ch d D f g G x k l ll m n ny p r rr s t th )
   )
   ;; rules will add - at syllable boundaries
   (
      ;; valid CC groups
      ( V C * [ b l ] V = - b l )
      ( V C * [ b r ] V = - b r )
      ( V C * [ k l ] V = - k l )
      ( V C * [ k r ] V = - k r )
      ( V C * [ k s ] V = - k s ) ; for words with "x"
      ( V C * [ d r ] V = - d r )
      ( V C * [ f l ] V = - f l )
      ( V C * [ f r ] V = - f r )
      ( V C * [ g l ] V = - g l )
      ( V C * [ g r ] V = - g r )
      ( V C * [ p l ] V = - p l )
      ( V C * [ p r ] V = - p r )
      ( V C * [ t l ] V = - t l )
      ( V C * [ t r ] V = - t r )

      ;; triphthongs
      ( [ i a i ] = i a i )  
      ( [ i a u ] = i a u )  
      ( [ u a i ] = u a i )  
      ( [ u a u ] = u a u )  
      ( [ i e i ] = i e i )  
      ( [ i e u ] = i e u )  
      ( [ u e i ] = u e i )  
      ( [ u e u ] = u e u )  
      ( [ i o i ] = i o i )  
      ( [ i o u ] = i o u )  
      ( [ u o i ] = u o i )  
      ( [ u o u ] = u o u )  
      ( [ i a1 i ] = i a1 i )  
      ( [ i a1 u ] = i a1 u )  
      ( [ u a1 i ] = u a1 i )  
      ( [ u a1 u ] = u a1 u )  
      ( [ i e1 i ] = i e1 i )  
      ( [ i e1 u ] = i e1 u )  
      ( [ u e1 i ] = u e1 i )  
      ( [ u e1 u ] = u e1 u )  
      ( [ i o1 i ] = i o1 i )  
      ( [ i o1 u ] = i o1 u )  
      ( [ u o1 i ] = u o1 i )  
      ( [ u o1 u ] = u o1 u )  

      ;; break invalid triphthongs
      ( IUT [ i a ]  = - i a )
      ( IUT [ i e ]  = - i e )
      ( IUT [ i o ]  = - i o )
      ( IUT [ u a ]  = - u a )
      ( IUT [ u e ]  = - u e )
      ( IUT [ u o ]  = - u o )
      ( IUT [ a i ]  = - a i )
      ( IUT [ e i ]  = - e i )
      ( IUT [ o i ]  = - o i )
      ( IUT [ a u ]  = - a u )
      ( IUT [ e u ]  = - e u )
      ( IUT [ o u ]  = - o u )
      ( IUT [ i u ]  = - i u )
      ( IUT [ u i ]  = - u i )
      ( IUT [ i a1 ]  = - i a1 )
      ( IUT [ i e1 ]  = - i e1 )
      ( IUT [ i o1 ]  = - i o1 )
      ( IUT [ u a1 ]  = - u a1 )
      ( IUT [ u e1 ]  = - u e1 )
      ( IUT [ u o1 ]  = - u o1 )
      ( IUT [ a1 i ]  = - a1 i )
      ( IUT [ e1 i ]  = - e1 i )
      ( IUT [ o1 i ]  = - o1 i )
      ( IUT [ a1 u ]  = - a1 u )
      ( IUT [ e1 u ]  = - e1 u )
      ( IUT [ o1 u ]  = - o1 u )
      ( IUT [ i u1 ]  = - i u1 )
      ( IUT [ u i1 ]  = - u i1 )

      ;; diphthongs
      ( [ i a ]  = i a )
      ( [ i e ]  = i e )
      ( [ i o ]  = i o )
      ( [ u a ]  = u a )
      ( [ u e ]  = u e )
      ( [ u o ]  = u o )
      ( [ a i ]  = a i )
      ( [ e i ]  = e i )
      ( [ o i ]  = o i )
      ( [ a u ]  = a u )
      ( [ e u ]  = e u )
      ( [ o u ]  = o u )
      ( [ i u ]  = i u )
      ( [ u i ]  = u i )
      ( [ i a1 ]  = i a1 )
      ( [ i e1 ]  = i e1 )
      ( [ i o1 ]  = i o1 )
      ( [ u a1 ]  = u a1 )
      ( [ u e1 ]  = u e1 )
      ( [ u o1 ]  = u o1 )
      ( [ a1 i ]  = a1 i )
      ( [ e1 i ]  = e1 i )
      ( [ o1 i ]  = o1 i )
      ( [ a1 u ]  = a1 u )
      ( [ e1 u ]  = e1 u )
      ( [ o1 u ]  = o1 u )
      ( [ u1 i ]  = u1 i )
      ( [ i1 u ]  = i1 u )
    
      ;; vowels preceeded by vowels are syllable breaks
      ;; triphthongs and diphthongs are dealt with above
      ( V [ a ]  = - a )
      ( V [ i ]  = - i )
      ( V [ u ]  = - u )
      ( V [ e ]  = - e )
      ( V [ o ]  = - o )
      ( V [ a1 ]  = - a1 )
      ( V [ e1 ]  = - e1 )
      ( V [ i1 ]  = - i1 )
      ( V [ o1 ]  = - o1 )
      ( V [ u1 ]  = - u1 )

      ;; if any consonant is followed by a vowel and there is a vowel before it, it is a syllable break
      ;; consonant clusters are dealt with above
      ( V C * [ b ] V = - b )
      ( V C * [ B ] V = - B )
      ( V C * [ ch ] V = - ch )
      ( V C * [ d ] V = - d )
      ( V C * [ D ] V = - D )
      ( V C * [ f ] V = - f )
      ( V C * [ g ] V = - g )
      ( V C * [ G ] V = - G )
      ( V C * [ x ] V = - x )
      ( V C * [ k ] V = - k )
      ( V C * [ l ] V = - l )
      ( V C * [ ll ] V = - ll )
      ( V C * [ m ] V = - m )
      ( V C * [ n ] V = - n )
      ( V C * [ ny ] V = - ny )
      ( V C * [ p ] V = - p )
      ( V C * [ r ] V = - r )
      ( V C * [ rr ] V = - rr )
      ( V C * [ s ] V = - s )
      ( V C * [ t ] V = - t )
      ( V C * [ th ] V = - th )

      ;; catch all consonants on their own (at end of a word)
      ;; vowels not preceded by vowels are just written as is
      ( [ b ] = b )
      ( [ B ] = B )
      ( [ ch ] = ch )
      ( [ d ] = d )
      (	[ D ] = D )
      ( [ f ] = f )
      ( [ g ] = g )
      (	[ G ] = G )
      ( [ x ] = x )
      ( [ k ] = k )
      ( [ l ] = l )
      ( [ ll ] = ll )
      ( [ m ] = m )
      ( [ n ] = n )
      ( [ ny ] = ny )
      ( [ p ] = p )
      ( [ r ] = r )
      ( [ rr ] = rr )
      ( [ s ] = s )
      ( [ t ] = t )
      ( [ th ] = th )
      ( [ a ]  =  a )
      ( [ i ]  =  i )
      ( [ u ]  =  u )
      ( [ e ]  =  e )
      ( [ o ]  =  o )
      ( [ a1 ]  =  a1 )
      ( [ i1 ]  =  i1 )
      ( [ u1 ]  =  u1 )
      ( [ e1 ]  =  e1 )
      ( [ o1 ]  =  o1 )
   )
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Stress assignment in unstressed words by rewrite rules
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; These rules assign stress to a vowel when it does not bear it 
;; This is called after "break_words_into_syl"

(lts.ruleset
 assign_stress_tovowels
 (
  (UV a i u e o ax)
  (V a1 i1 u1 e1 o1  a i u e o ax)
  (V1 a1 i1 u1 e1 o1)
  (VNS n s a i u e o ax)
  (C b B ch d D f g G j k l ll m n ny p r rr s t th x )
  (VC b B ch d D f g G j k l ll m n ny p r rr s t th x a1 i1 u1 e1 o1  a i u e o ax)
  (ANY b B ch d D f g G j k l ll m n ny p r rr s t th x - a1 i1 u1 e1 o1  a i u e o ax)
  (notNS b B ch d D f g G j k l ll m ny p r rr t th x )
  (iu i u )
  (aeo a e o)
  )
 (
  ;; consonants to themselves
  ( [ b ] = b )
  ( [ B ] = B )
  ( [ d ] = d )
  ( [ D ] = D )
  ( [ ch ] = ch )
  ( [ f ] = f )
  ( [ g ] = g )
  ( [ G ] = G )
  ( [ j ] = j )
  ( [ k ] = k )
  ( [ l ] = l )
  ( [ ll ] = ll )
  ( [ m ] = m )
  ( [ n ] = n )
  ( [ ny ] = ny )
  ( [ p ] = p )
  ( [ r ] = r )
  ( [ rr ] = rr )
  ( [ s ] = s )
  ( [ t ] = t )
  ( [ th ] = th )
  ( [ x ] = x )
  ( [ - ] = - )

  ;; stressed vowels to themselves
  ( [ a1 ] = a1 )
  ( [ i1 ] = i1 )
  ( [ u1 ] = u1 )
  ( [ e1 ] = e1 )
  ( [ o1 ] = o1 )

  ( V1 ANY * [ a ] = a )
  ( V1 ANY * [ e ] = e )
  ( V1 ANY * [ i ] = i )
  ( V1 ANY * [ o ] = o )
  ( V1 ANY * [ u ] = u )
  ( [ a ] ANY * V1 = a )
  ( [ e ] ANY * V1 = e )
  ( [ i ] ANY * V1 = i )
  ( [ o ] ANY * V1 = o )
  ( [ u ] ANY * V1 = u )
  
  ;; we'll only get here when the vowel is in an unstressed word
  ( [ a ] VC * -  VC * - = a )
  ( [ e ] VC * -  VC * - = e )
  ( [ i ] VC * -  VC * - = i )
  ( [ o ] VC * -  VC * - = o )
  ( [ u ] VC * -  VC * - = u )

  ( [ a ] ANY * - VC * aeo i # = a )
  ( [ e ] ANY * - VC * aeo i # = e )
  ( [ i ] ANY * - VC * aeo i # = i )
  ( [ o ] ANY * - VC * aeo i # = o )
  ( [ u ] ANY * - VC * aeo i # = u )

  ( [ a ] VC * - VC * VNS # = a1 )
  ( [ e ] VC * - VC * VNS # = e1 )
  ( [ o ] VC * - VC * VNS # = o1 )
  ( [ i ] aeo C * - VC * VNS # = i )
  ( [ u ] aeo C * - VC * VNS # = u )
  ( aeo [ i ] C * - VC * VNS # = i )
  ( aeo [ u ] C * - VC * VNS # = u )
  ( [ u ] C * - VC * VNS # = u1 )
  ( [ i ] C * - VC * VNS # = i1 )

  ( [ a ] i # = a1 )
  ( [ e ] i # = e1 )
  ( [ o ] i # = o1 )

  ;; stress on previous syllable
  ( - VC * [ a ] VC * VNS # = a )
  ( - VC * [ e ] VC * VNS # = e )
  ( - VC * [ i ] VC * VNS # = i )
  ( - VC * [ o ] VC * VNS # = o )
  ( - VC * [ u ] VC * VNS # = u )
  ( - VC * [ a ] # = a )
  ( - VC * [ e ] # = e )
  ( - VC * [ i ] # = i )
  ( - VC * [ o ] # = o )
  ( - VC * [ u ] # = u )

  ;; stress on final syllable
  ( [ a ] VC * # = a1 )
  ( [ e ] VC * # = e1 )
  ( [ o ] VC * # = o1 )
  ( aeo [ i ] VC * # = i )
  ( aeo [ u ] VC * # = u )
  ( [ i ] aeo VC * # = i )
  ( [ u ] aeo VC * # = u )
  ( [ i ] VC * # = i1 )
  ( [ u ] VC * # = u1 )

  ( [ a ] = a )
  ( [ e ] = e )
  ( [ i ] = i )
  ( [ o ] = o )
  ( [ u ] = u )
  
))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Postlexical Rules 
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (JuntaDeAndalucia_es::postlex_rule1 utt)
  "(JuntaDeAndalucia_es::postlex_rule1 utt)
A postlexical rule form correcting phenomena over word boundaries."
  (mapcar
   (lambda (s)
     ;; do something
     )
   (utt.relation.items utt 'Segment))
   utt)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Lexicon definition
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(lex.create "JuntaDeAndalucia_es")
(lex.set.phoneset "JuntaDeAndalucia_es")
(lex.set.lts.method 'Indisys_MP_es_lts)
(lex.set.lts.ruleset 'follow_spanish_phonetic_rules)
;;; If you have a compiled lexicon uncomment this
;(lex.set.compile.file (path-append JuntaDeAndalucia_es_pa_dir "festvox/JuntaDeAndalucia_es_lex.out"))
(JuntaDeAndalucia_es_addenda)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Lexicon setup
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (JuntaDeAndalucia_es_pa::select_lexicon)
  "(JuntaDeAndalucia_es_pa::select_lexicon)
Set up the lexicon for JuntaDeAndalucia_es."
  (lex.select "JuntaDeAndalucia_es")

  ;; Post lexical rules
  (set! postlex_rules_hooks (list JuntaDeAndalucia_es::postlex_rule1))
)

(define (JuntaDeAndalucia_es_pa::reset_lexicon)
  "(JuntaDeAndalucia_es_pa::reset_lexicon)
Reset lexicon information."
  t
)

(provide 'JuntaDeAndalucia_es_pa_lexicon)

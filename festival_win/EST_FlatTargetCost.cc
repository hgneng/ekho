/*************************************************************************/
/*                                                                       */
/*                Centre for Speech Technology Research                  */
/*                 (University of Edinburgh, UK) and                     */
/*                           Rob Clark                                   */
/*                         Copyright (c) 2006                            */
/*                         All Rights Reserved.                          */
/*                                                                       */
/*  Permission is hereby granted, free of charge, to use and distribute  */
/*  this software and its documentation without restriction, including   */
/*  without limitation the rights to use, copy, modify, merge, publish,  */
/*  distribute, sublicense, and/or sell copies of this work, and to      */
/*  permit persons to whom this work is furnished to do so, subject to   */
/*  the following conditions:                                            */
/*                                                                       */
/*   1. The code must retain the above copyright notice, this list of    */
/*      conditions and the following disclaimer.                         */
/*   2. Any modifications must be clearly marked as such.                */
/*   3. Original authors' names are not deleted.                         */
/*   4. The authors' names are not used to endorse or promote products   */
/*      derived from this software without specific prior written        */
/*      permission.                                                      */
/*                                                                       */
/*  THE UNIVERSITY OF EDINBURGH AND THE CONTRIBUTORS TO THIS WORK        */
/*  DISCLAIM ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING      */
/*  ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS. IN NO EVENT   */
/*  SHALL THE UNIVERSITY OF EDINBURGH NOR THE CONTRIBUTORS BE LIABLE     */
/*  FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES    */
/*  WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN   */
/*  AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION,          */
/*  ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF       */
/*  THIS SOFTWARE.                                                       */
/*                                                                       */
/*************************************************************************/
/*                                                                       */
/*                        Author: Rob Clark                              */
/*                          Date: June 2006                              */
/* --------------------------------------------------------------------- */
/*                                                                       */
/*                                                                       */
/*                                                                       */
/*                                                                       */
/*                                                                       */
/*                                                                       */
/*************************************************************************/

#include <iostream>
#include "festival.h"
#include "ling_class/EST_Item.h"
#include "EST_FlatTargetCost.h"
#include "siod.h"

static const int simple_phone(const EST_String&);
static const int simple_id(const EST_String&);
static const int simple_pos(const EST_String &s);
static const int simple_punc(const EST_String &s);
static const int get_bad_f0(const EST_Item *seg);
static const EST_Item* tc_get_syl(const EST_Item *seg);
static const EST_Item* tc_get_word(const EST_Item *seg);

static EST_TStringHash<int> phonehash(10);
static int phone_count=0;

//VAL_REGISTER_TYPE(tcdata,TCData)

/*
 *  BASE CLASS:  EST_TargetCost
 */


/* Individual cost functions */




TCData *EST_FlatTargetCost::flatpack(EST_Item *seg) const
{

  const EST_Item *syl, *nsyl, *nnsyl, *word;

  TCData *f =new TCData(TCHI_LAST);

  syl=tc_get_syl(seg);
  nsyl=tc_get_syl(seg->next()); 
  if(seg->next()->next())
    nnsyl=tc_get_syl(seg->next()->next());
  else nnsyl = 0;

  // This segment features

  //cout << "SEG: " << seg->S("name") << " is vowel: " 
  //    << ph_is_vowel(seg->S("name")) << endl;

  if(ph_is_vowel(seg->S("name")))
    (*f)[VOWEL]=1;
  else
    (*f)[VOWEL]=0;

  //cout << "SEG: " << seg->S("name") << " is sil: " 
  //     << ph_is_silence(seg->S("name")) << endl;

  if(ph_is_silence(seg->S("name")))
    (*f)[SIL]=1;
  else
    (*f)[SIL]=0;

  if(seg->f_present("bad_dur"))
    (*f)[BAD_DUR]=1;
  else 
    (*f)[BAD_DUR]=0;

  if(seg->next()->f_present("bad_dur"))
    (*f)[NBAD_DUR]=1;
  else 
    (*f)[NBAD_DUR]=0;

  (*f)[BAD_F0]=get_bad_f0(seg);


  // This segments syl features

  if(syl)
    {
      (*f)[SYL]=simple_id(syl->S("id"));
      (*f)[SYL_STRESS]=syl->I("stress");
      //cout << "syl id: " <<  simple_id(syl->S("id"))
      //<< " stress: " << syl->I("stress") << endl;
    }
  else 
    {
      (*f)[SYL]=0;
      (*f)[SYL_STRESS]=0;
      //cout << "no syl present " << endl;

    }


  // Next segment features

  //cout << "NSEG: " << seg->next()->S("name") << " is sil: " 
  //    << ph_is_silence(seg->next()->S("name")) << endl;

  if(ph_is_silence(seg->next()->S("name")))
    (*f)[N_SIL]=1;
  else
    (*f)[N_SIL]=0;

  //cout << "NSEG: " << seg->next()->S("name") << " is vowel: " 
  //   << ph_is_vowel(seg->next()->S("name")) << endl;

  if(ph_is_vowel(seg->next()->S("name")))
    (*f)[N_VOWEL]=1;
  else
    (*f)[N_VOWEL]=0;

  // Next seg syl features
  if(nsyl)
    {
      (*f)[NSYL]=simple_id(nsyl->S("id"));
      (*f)[NSYL_STRESS]=nsyl->I("stress");
      //cout << "nsyl stress: " << nsyl->I("stress") << endl;
    }
  else
    {
      (*f)[NSYL]=0;
      (*f)[NSYL_STRESS]=0;
      //cout << "no nsyl: " << endl;
    }

  if(seg->next()->next())
    {
      //cout << "RC: " << seg->next()->next()->S("name")
      //<< " " << simple_phone(seg->next()->next()->S("name"))
      //	   << endl;
      (*f)[RC]=simple_phone(seg->next()->next()->S("name"));
      (*f)[NNBAD_DUR]=seg->next()->next()->f_present("bad_dur");
    }
  else
    {
      //cout << "NO RC\n";
      (*f)[RC]=0;
      (*f)[NNBAD_DUR]=0;
    }

  // Next next seg syl features.
  if(nnsyl)
    {
      (*f)[NNSYL]=simple_id(nnsyl->S("id"));
    }
  else
    (*f)[NNSYL]=0;

  // Prev seg syl feature
  if(seg->prev())
    {
      (*f)[LC]=simple_phone(seg->prev()->S("name"));
      (*f)[PBAD_DUR]=seg->prev()->f_present("bad_dur");
    }
  else
    {
    (*f)[LC]=0;
    (*f)[PBAD_DUR]=0;
    }  

  if(seg->prev() && (syl=tc_get_syl(seg->prev())))
    (*f)[PSYL]=simple_id(syl->S("id"));
  else
    (*f)[PSYL]=0;

  // seg word feature
  if(word=tc_get_word(seg))
    (*f)[CWORD]=simple_id(word->S("id"));
  else
    (*f)[CWORD]=0;
  

  // Next seg word features
  if(word=tc_get_word(seg->next()))
    (*f)[NWORD]=simple_id(word->S("id"));
  else
    (*f)[NWORD]=0;

  // next next seg word feature
  if(seg->next()->next() && (word=tc_get_word(seg->next()->next())))
    (*f)[NNWORD]=simple_id(word->S("id"));
  else
    (*f)[NNWORD]=0;

  // Prev seg word feature
    if(seg->prev() && (word=tc_get_word(seg->prev())))
      (*f)[CPWORD]=simple_id(word->S("id"));
    else
      (*f)[CPWORD]=0;


  // segs sylpos
  (*f)[SYLPOS]=0; // medial
  if( f->a_no_check(SYL)!= f->a_no_check(NSYL) )
    (*f)[SYLPOS]=1;  // inter
  else if( f->a_no_check(SYL)!= f->a_no_check(PSYL) )
    (*f)[SYLPOS]=2; // initial
  else if( f->a_no_check(NSYL) != f->a_no_check(NNSYL) )
    (*f)[SYLPOS]=3; // final

  // segs wordpos
  (*f)[WORDPOS]=0; // medial
  if( f->a_no_check(CWORD)!= f->a_no_check(NWORD) )
    (*f)[WORDPOS]=1;  // inter
  else if( f->a_no_check(CWORD)!= f->a_no_check(CPWORD) )
    (*f)[WORDPOS]=2; // initial
  else if( f->a_no_check(NWORD) != f->a_no_check(NNWORD) )
    (*f)[WORDPOS]=3; // final

  // pbreak
  if ( word = tc_get_word(seg) )
    {
      if ( word->S("pbreak") == "NB" )
	(*f)[PBREAK]=0;
      else if ( word->S("pbreak") == "B" )
	(*f)[PBREAK]=1;
      else
	(*f)[PBREAK]=2;
    }
  else
    (*f)[PBREAK]=-1;

  // seg punc and pos
  if(word = tc_get_word(seg))
    {
      (*f)[POS]=simple_pos(word->S("pos"));
      (*f)[PUNC]=simple_punc(parent(word,"Token")->S("punc","NONE"));
    }
  else
    {
      (*f)[POS]=-1;
      (*f)[PUNC]=-1;
    }

  // next seg punc and pos
  if (word = tc_get_word(seg->next()))
    {
      (*f)[NPOS]=simple_pos(word->S("pos"));
      (*f)[NPUNC]=simple_punc(parent(word,"Token")->S("punc","NONE"));
    }
  else
    {
      (*f)[NPOS]=-1;
      (*f)[NPUNC]=-1; 
    }

  return f;
  //seg->set_val("tcdata",est_val(f));  // copied?

}
      

float EST_FlatTargetCost::stress_cost() const
{

  if( t->a_no_check(VOWEL) && ! t->a_no_check(SIL)) 
    {
      // Can't assume candidate and target identities are the same
      // (because of backoff to a silence for example)
      if( c->a_no_check(SYL) == 0  ||  c->a_no_check(NSYL) )
	return 1.0;
      
      if ( t->a_no_check(SYL_STRESS) != c->a_no_check(SYL_STRESS) )
	return 1.0;

      if ( t->a_no_check(NSYL_STRESS) != c->a_no_check(NSYL_STRESS) )
	return 1.0;

    }

      return 0.0;

}


float EST_FlatTargetCost::position_in_phrase_cost() const
{
  
  if ( !t->a_no_check(CWORD) && !c->a_no_check(CWORD) )
    return 0;
  if ( !t->a_no_check(CWORD) || !c->a_no_check(CWORD) )
    return 1;

  return ( t->a_no_check(PBREAK) == c->a_no_check(PBREAK) ) ? 0 : 1;
}

float EST_FlatTargetCost::punctuation_cost() const
{

  float score = 0.0;

  if ( (t->a_no_check(CWORD) && !c->a_no_check(CWORD)) 
       || (!t->a_no_check(CWORD) && c->a_no_check(CWORD)) )
    score += 0.5;
  else
    if (t->a_no_check(CWORD) && c->a_no_check(CWORD))
      if ( t->a_no_check(PUNC) != c->a_no_check(PUNC) )
	score += 0.5;
  
  if ( (t->a_no_check(NWORD) && !c->a_no_check(NWORD)) 
       || (!t->a_no_check(NWORD) && c->a_no_check(NWORD)) )
    score += 0.5;
  else
    if(t->a_no_check(NWORD) && c->a_no_check(NWORD))
      if ( t->a_no_check(NPUNC) != c->a_no_check(NPUNC) )
	score += 0.5;
  
  return score;

}


float EST_FlatTargetCost::partofspeech_cost() const
{
  // Compare left phone half of diphone
  if(!t->a_no_check(CWORD) && !c->a_no_check(CWORD))
    return 0;
  if(!t->a_no_check(CWORD) || !c->a_no_check(CWORD))
    return 1;
  if( t->a_no_check(POS) != c->a_no_check(POS) )
    return 1;

  // Compare right phone half of diphone
  if(!t->a_no_check(NWORD) && !c->a_no_check(NWORD))
    return 0;
  if(!t->a_no_check(NWORD) || !c->a_no_check(NWORD))
    return 1;
  if( t->a_no_check(NPOS) != c->a_no_check(NPOS) )
    return 1;

  return 0;
}


float EST_FlatTargetCost::bad_duration_cost() const
{
  // bad_dur may at some stage be set on a target for resynthesis purposes.
  if( c->a_no_check(BAD_DUR) != t->a_no_check(BAD_DUR) )
    return 1.0;
  
  if( c->a_no_check(NBAD_DUR) != t->a_no_check(NBAD_DUR) )
    return 1.0;

  // If the segments next to these segments are bad, then these ones are probably wrong too!
  if( c->a_no_check(PBAD_DUR) != t->a_no_check(PBAD_DUR) )
    return 1.0;
  if( c->a_no_check(NNBAD_DUR) != t->a_no_check(NNBAD_DUR) )
    return 1.0;
  
  return 0.0;
}


/*
 *  DERIVED CLASS: EST_FlatTargetCost
 *
 *  This is CSTR's proposed default target cost flat packed. Nothing
 *  special, if you think you can do better derive your own class.
 */

float EST_FlatTargetCost::operator()(const TCData* targ, const TCData* cand) const 
{ 
  set_t_and_c(targ,cand);
  score = 0.0;
  weight_sum = 0.0;

  score += add_weight(10.0)*stress_cost();
  score += add_weight(5.0)*position_in_syllable_cost();
  score += add_weight(5.0)*position_in_word_cost();
  score += add_weight(6.0)*partofspeech_cost();
  score += add_weight(15.0)*position_in_phrase_cost();
  score += add_weight(4.0)*left_context_cost();
  score += add_weight(3.0)*right_context_cost();

  score /= weight_sum;

  // These are considered really bad, and will result in a score > 1.
  score += 10.0*bad_duration_cost(); // see also join cost.
  score += 10.0*bad_f0_cost();
  score += 10.0*punctuation_cost();


  return score ;
}



/* 
 *   Auxillary target cost functions
 */

static const int simple_phone(const EST_String &phone)
{
  if(phonehash.present(phone))
    return phonehash.val(phone);
  
  phonehash.add_item(phone,++phone_count);
  return phone_count;

}

static const int simple_id(const EST_String &id)
{
  return id.after("_").Int();
}


static const int simple_punc(const EST_String &punc)
{
  if ( punc == "NONE")
    return 0;
  else if ( punc == "," || punc == ":" || punc == ";" )
    return 1;
  else if ( punc == "\"" || punc == "'" || punc == "-" )
    return 1;
  else if ( punc == "(" || punc == ")" )
    return 1;
  else if ( punc == ".")
    return 2;
  else if ( punc == "?")
    return 3;
  else
    return 0;
}

static const int simple_pos(const EST_String &s)
{
  if( s == "nn" || s == "nnp" || s == "nns" || s == "nnps" || s == "fw" || s == "sym" || s == "ls")
    return 1;
  if( s == "vbd" || s == "vb" || s == "vbn" || s == "vbz" || s == "vbp" || s == "vbg")
    return 2;
  if( s == "jj" || s == "jjr" || s == "jjs" || s == "1" || s == "2" || s == "rb" || 
      s == "rp" || s == "rbr" || s == "rbs")
    return 3;
  return 0;
 }

static const int get_bad_f0(const EST_Item *seg)
{
  // by default, the last element of join cost coef vector is
  // the f0 (i.e. fv->a_no_check( fv->n()-1 ) )

  EST_String left(seg->S("name"));
  EST_String right(seg->next()->S("name"));
  
  EST_FVector *fv = 0;
  int penalty = 0;
  
  if( seg->f_present("midcoef") && 
      ( ph_is_vowel( left )
	|| ph_is_approximant( left )
	|| ph_is_liquid( left )
	|| ph_is_nasal( left ) )){
    fv = fvector( seg->f("midcoef") );
    if( fv->a_no_check(fv->n()-1) == -1.0 ) // means unvoiced
      penalty += 1;
  }
  
  if( seg->next()->f_present("midcoef") && 
      ( ph_is_vowel( right )
	|| ph_is_approximant( right )
	|| ph_is_liquid( right )
	|| ph_is_nasal( right ) ) ){
    fv = fvector( seg->next()->f("midcoef") );
    if( fv->a_no_check(fv->n()-1) == -1.0 ) // means unvoiced 
      penalty += 1;
  }

  return penalty/2; 
}

static const EST_Item *tc_get_syl(const EST_Item *seg)
{
  //  if(!seg)
  //  return 0;
  
  return parent(seg,"SylStructure");
}

static const EST_Item *tc_get_word(const EST_Item *seg)
 {
   // if(!seg)
   //  return 0;
   const EST_Item *syl = tc_get_syl(seg);
   
   if(syl)
     return parent(syl,"SylStructure");
   else
     return 0;
 }



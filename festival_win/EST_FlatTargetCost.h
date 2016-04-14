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
/*                          Author: Rob Clark                            */
/*                            Date: June 2006                            */
/* --------------------------------------------------------------------- */
/*                                                                       */
/*                                                                       */
/*                                                                       */
/*                                                                       */
/*************************************************************************/


#ifndef __EST_TARGETCOST_FLAT_H__
#define __EST_TARGETCOST_FLAT_H__

#include "EST_THash.h"
#include "EST_TargetCost.h" 

enum tcdata_t 
{
  VOWEL, SIL,BAD_DUR, NBAD_DUR, BAD_F0,
  SYL, SYL_STRESS, N_SIL, N_VOWEL,
  NSYL, NSYL_STRESS,
  RC, NNBAD_DUR, NNSYL, LC, PBAD_DUR,
  PSYL, CWORD, NWORD, NNWORD, CPWORD,
  SYLPOS, WORDPOS, PBREAK, 
  POS, PUNC, NPOS, NPUNC,
  TCHI_LAST
} ;


typedef EST_IVector TCData;
VAL_REGISTER_TYPE_DCLS(ivector,TCData)

typedef EST_THash<EST_Item*,TCData*> TCDataHash; 


/*
 *  DERIVED CLASS: EST_FlatTargetCost
 */
class EST_FlatTargetCost : public EST_TargetCost {

 public:
  EST_FlatTargetCost() : li(0){};


 private:
  mutable const TCData *t;
  mutable const TCData *c;
  mutable const EST_Item *li; 

  inline void set_t_and_c(const TCData* targ, const TCData* cand) const
    { 
      t = targ;
      c = cand;
    }

  float stress_cost() const;
  inline float position_in_syllable_cost() const
  { return ( t->a_no_check(SYLPOS) == c->a_no_check(SYLPOS) ) ? 0 : 1; }
  inline float position_in_word_cost() const
  { return ( t->a_no_check(WORDPOS) == c->a_no_check(WORDPOS) ) ? 0 : 1; }
  float position_in_phrase_cost() const;
  float punctuation_cost() const;
  float partofspeech_cost() const;
  inline float left_context_cost() const
  { return ( t->a_no_check(LC) == c->a_no_check(LC) ) ? 0 : 1; }
  inline float right_context_cost() const
  { return ( t->a_no_check(RC) == c->a_no_check(RC) ) ? 0 : 1; }
  float bad_duration_cost() const;
  inline float bad_f0_cost() const
  { return float(c->a_no_check(BAD_F0)) / 2.0; }



 public:
  float operator()(const EST_Item* targ, const EST_Item* cand) const
  { EST_error("EST_FlatTargetCost operator() called with EST_Items\n");
    return 1; }
  float operator()(const TCData *targ, const TCData *cand) const;
  const bool is_flatpack() const { return true; }
  TCData *flatpack(EST_Item *seg) const;


};





#endif // __EST_TARGETCOST_H__






/*

 This file is part of FFTS -- The Fastest Fourier Transform in the South

 Copyright (c) 2012, Anthony M. Blake <amb@anthonix.com>
 Copyright (c) 2012, The University of Waikato

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

typedef struct _sym_t {
    int c;
    struct _sym_t *pPrev, *pNext;
    struct _seq_rule_t *r;
    int offset;
} sym_t;

typedef struct _seq_rule_t {
    int c;
    sym_t *ss;
    struct _seq_rule_t *pPrev, *pNext;
    int count;
    int length;
} seq_rule_t;

void sym_tail_insert(sym_t **ss, sym_t *s)
{
    if (!*ss) {
        *ss = s;
        s->pPrev = s->pNext = NULL;
    } else {
        while (*ss) {
            s->pPrev = *ss;
            ss = &(*ss)->pNext;
        }

        *ss = s;
    }
}

sym_t* sym_init(int c)
{
    sym_t *s;

    s = (sym_t*) malloc(sizeof(*s));
    if (!s) {
        return NULL;
    }

    s->c = c;
    s->pPrev = s->pNext = NULL;
    s->r = NULL;

    return s;
}

sym_t* sym_init_from_sym(sym_t *s2)
{
    sym_t *s;

    s = (sym_t*) malloc(sizeof(*s));
    if (!s) {
        return NULL;
    }

    s->c = s2->c;
    s->pPrev = s->pNext = NULL;
    s->r = s2->r;
    s->offset = s2->offset;

    return s;
}

seq_rule_t* seq_init_rule(int c)
{
    seq_rule_t *G;

    G = (seq_rule_t *)malloc(sizeof(*G));
    if (!G) {
        return NULL;
    }

    G->c = c;
    G->count = 2;
    G->ss = NULL;
    G->pPrev = NULL;
    G->pNext = NULL;

    return G;
}

seq_rule_t* seq_grammer_insert_new_rule(seq_rule_t *G, char r, sym_t *a, sym_t *b)
{
    sym_t *sa, *sb;

    while (G->pNext) {
        G = G->pNext;
    }

    G->pNext = seq_init_rule(r);
    if (!G->pNext) {
        return NULL;
    }

    sa = sym_init_from_sym(a);
    if (!sa) {
        goto cleanup_pnext;
    }

    sb = sym_init_from_sym(b);
    if (!sb) {
        goto cleanup_sa;
    }

    sb->offset = sb->offset - sa->offset;
    sa->offset = 0;
    sym_tail_insert(&G->pNext->ss, sa);
    sym_tail_insert(&G->pNext->ss, sb);
    return G->pNext;

cleanup_sa:
    free(sa);

cleanup_pnext:
    free(G->pNext);
    G->pNext = NULL;

    return NULL;
}

sym_t* sym_match_digram(sym_t *s, sym_t *term, sym_t *a, sym_t *b)
{
    while (s != term) {
        if (s->c == a->c && s->pNext->c == b->c &&
                s->pNext->offset - s->offset == b->offset-a->offset) {
            return s;
        }

        s = s->pNext;
    }

    return NULL;
}

seq_rule_t* seq_match_digram(seq_rule_t *R, sym_t *a, sym_t *b)
{
    while (R) {
        if (R->ss->c == a->c && R->ss->pNext->c == b->c &&
                R->ss->pNext->offset - R->ss->offset == b->offset - a->offset) {
            return R;
        }

        R = R->pNext;
    }

    return NULL;
}

sym_t* sym_tail(sym_t *s)
{
    while (s->pNext) {
        s = s->pNext;
    }

    return s;
}

int sym_count(sym_t *s)
{
    int count = 0;

    while (s) {
        count++;
        s = s->pNext;
    }

    return count;
}

sym_t* sym_copylist(sym_t *s)
{
    sym_t *head = NULL;
    sym_t *prev = head;

    while (s) {
        sym_t *copy = sym_init_from_sym(s);
        if (!copy) {
            return NULL;
        }

        copy->pPrev = prev;

        if (prev) {
            prev->pNext = copy;
        }

        if (!head) {
            head = copy;
        }

        prev = copy;
        s = s->pNext;
    }

    return head;
}

void seq_enforce_uniqueness(seq_rule_t *G)
{
    seq_rule_t *R = G;//->pNext;
    seq_rule_t **ppr = &G->pNext;

    while (R) {
        if (R == G || R->count > 1) {
            sym_t *s = R->ss;
            sym_t **pp = &R->ss;

            while (s) {
                if (s->r && s->r->count == 1) {
                    sym_t *temp_itr;

                    *pp = s->r->ss;

                    temp_itr = s->r->ss;
                    while (temp_itr) {
                        temp_itr->offset += s->offset;
                        temp_itr = temp_itr->pNext;
                    }

                    s->r->ss->pPrev = s->pPrev;
                    if (s->pNext) {
                        s->pNext->pPrev = sym_tail(s->r->ss);
                    }

                    sym_tail(s->r->ss)->pNext = s->pNext;
                    s = s->r->ss;
                    continue;
                }

                pp = &s->pNext;
                s = s->pNext;
            }

            ppr = &R->pNext;
        } else {
            *ppr = R->pNext;
        }

        R = R->pNext;
    }
}

void seq_merge_small_rules(seq_rule_t *G, int thresh)
{
    seq_rule_t *R = G;

    while (R) {
        if (sym_count(R->ss) <= thresh) {
            //printf("count %d > %d for %d\n", sym_count(R->ss), thresh, R->c);
            sym_t *s = R->ss;
            sym_t **pp = &R->ss;

            while (s) {
                if (s->r) {
                    sym_t *copylist;
                    sym_t *copylist_itr;

                    s->r->count--;

                    copylist = sym_copylist(s->r->ss);
                    if (!copylist) {
                        return;
                    }

                    copylist_itr = copylist;
                    while (copylist_itr) {
                        copylist_itr->offset += s->offset;
                        copylist_itr = copylist_itr->pNext;
                    }

                    *pp = copylist;
                    copylist->pPrev = s->pPrev;
                    if (s->pNext) {
                        s->pNext->pPrev = sym_tail(copylist);
                    }

                    sym_tail(copylist)->pNext = s->pNext;
                    pp = &(sym_tail(copylist)->pNext);
                    s = sym_tail(copylist)->pNext;
                    continue;
                }

                pp = &s->pNext;
                s = s->pNext;
            }
        }

        R = R->pNext;
    }

    seq_enforce_uniqueness(G);
}

void seq_extract_hierarchy(seq_rule_t *G)
{
    int next_rule = -2;
    sym_t *cursym = G->ss;

    while (cursym) {
        sym_t *m = NULL;
        seq_rule_t *mr = NULL;

        if (cursym->pPrev && cursym->pPrev->pPrev) {
            mr = seq_match_digram(G->pNext, cursym->pPrev, cursym);
            if (mr) {
                if (cursym->pPrev->r) {
                    cursym->pPrev->r->count--;
                }

                if(cursym->r) {
                    cursym->r->count--;
                }

                mr->count++;

                cursym->pPrev->r = mr;
                cursym->pPrev->c = mr->c;
                cursym->pPrev->pNext = cursym->pNext;
                cursym->pNext->pPrev = cursym->pPrev;
                cursym = cursym->pPrev;
            }

            m = sym_match_digram(G->ss, cursym->pPrev->pPrev, cursym->pPrev, cursym);
            if (m) {
                seq_rule_t *newr;

                if (cursym->pPrev->r) {
                    cursym->pPrev->r->count--;
                }

                if (cursym->r) {
                    cursym->r->count--;
                }

                newr = seq_grammer_insert_new_rule(G, next_rule, m, m->pNext);
                if (!newr) {
                    return;
                }

                m->r = newr;
                m->c = next_rule;
                m->pNext = m->pNext->pNext;
                m->pNext->pPrev = m;

                cursym->pPrev->r = newr;
                cursym->pPrev->c = next_rule;
                cursym->pPrev->pNext = cursym->pNext;
                cursym->pNext->pPrev = cursym->pPrev;
                cursym = cursym->pPrev;

                next_rule--;
            }
        }

        if (!m && !mr) {
            cursym = cursym->pNext;
        }
    }

    seq_enforce_uniqueness(G);
    seq_merge_small_rules(G, 2);
//	seq_enforce_uniqueness(G);
}

void seq_compute_lengths(seq_rule_t *G)
{
    seq_rule_t *R = G->pNext;
    sym_t *s;
    int sum;

    while (R) {
        sum = 0;
        s = R->ss;

        while (s) {
            if (s->c >= 0) {
                if (s->offset + s->c > sum) {
                    sum = s->offset + s->c;
                }
            }

            if (s->c < 0) {
                if (s->offset + s->r->length > sum) {
                    sum = s->offset + s->r->length;
                }
            }

            s = s->pNext;
        }

        R->length = sum;
        R = R->pNext;
    }

    sum = 0;
    s = G->ss;

    while (s) {
        if (s->c >= 0) {
            if (s->offset + s->c > sum) {
                sum = s->offset + s->c;
            }
        }

        if (s->c < 0) {
            if (s->offset + s->r->length > sum) {
                sum = s->offset + s->r->length;
            }
        }

        s = s->pNext;
    }

    G->length = sum;
}
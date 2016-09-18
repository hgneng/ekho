/*
  MusicXML Library
  Copyright (C) Grame 2006-2013

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.

  Grame Research Laboratory, 11, cours de Verdun Gensoul 69002 Lyon - France
  research@grame.fr
*/

#include "rational.h"
#include <sstream>
#include <cmath>
#include <stdlib.h>
#include <string.h>

string rational::toString() const
{
    ostringstream res;
    res << fNumerator;
    res << "/";
    res << fDenominator;
    return res.str();
}

rational::operator string() const
{
	return toString();
}

rational::operator double() const
{ 
	return toDouble(); 
}

rational::operator float() const
{ 
	return toFloat(); 
}

rational::operator int() const
{ 
	const double x = toDouble();
	return ((int)floor(x + 0.5f));
}

rational::rational(const string &str)
{
    const char *cstr;
    cstr = str.c_str();
    const char *denom;
    denom = strstr(cstr,"/");
    if (denom) ++denom;
    fNumerator = atol(cstr);
    if (denom) fDenominator = atol(denom);
}

rational::rational(long int num, long int denom) : fNumerator(num), fDenominator(denom)
{
    // don't allow zero denominators!
    if (fDenominator == 0) fDenominator = 1;
}

rational::rational(const rational& d)
{
    fNumerator = d.fNumerator;
    fDenominator = d.fDenominator;
}

rational rational::operator +(const rational &dur) const {
    return rational(fNumerator * dur.fDenominator + dur.fNumerator * fDenominator, fDenominator * dur.fDenominator);
}

rational rational::operator -(const rational &dur) const {
    return rational(fNumerator * dur.fDenominator - dur.fNumerator * fDenominator, fDenominator * dur.fDenominator);
}

rational rational::operator *(const rational &dur) const {
    return rational(fNumerator * dur.fNumerator, fDenominator * dur.fDenominator);
}

rational rational::operator /(const rational &dur) const {
    return rational(fNumerator * dur.fDenominator, fDenominator * dur.fNumerator);
}

rational rational::operator *(int num) const {
	return rational(fNumerator * num, fDenominator);
}

rational rational::operator /(int num) const {
	return rational(fNumerator, fDenominator * num);
}

rational& rational::operator +=(const rational &dur)
{
    if(fDenominator == dur.fDenominator) {
		fNumerator += dur.fNumerator;
	} else {
		fNumerator = fNumerator * dur.fDenominator + dur.fNumerator * fDenominator;
		fDenominator *= dur.fDenominator;
	}
    return (*this);
}

rational& rational::operator -=(const rational &dur)
{
	if(fDenominator == dur.fDenominator) {
		fNumerator -= dur.fNumerator;
	} else {
		fNumerator = fNumerator * dur.fDenominator - dur.fNumerator * fDenominator;
		fDenominator *= dur.fDenominator;
    }
	return (*this);
}

rational& rational::operator *=(const rational &dur)
{
    fNumerator   *= dur.fNumerator;
    fDenominator *= dur.fDenominator;
    return (*this);
}

rational& rational::operator /=(const rational &dur)
{
    fNumerator   *= dur.fDenominator;
    fDenominator *= dur.fNumerator;
    return (*this);
}

rational& rational::operator =(const rational& dur) {
    fNumerator   = dur.fNumerator;
    fDenominator = dur.fDenominator;
    return (*this);
}

bool rational::operator >(const rational &dur) const
{
    // a/b > c/d if and only if a * d > b * c.
    return ((fNumerator * dur.fDenominator) > (fDenominator * dur.fNumerator));
}

bool rational::operator <(const rational &dur) const
{
    // a/b < c/d if and only if a * d < b * c.
    return ((fNumerator * dur.fDenominator) < (fDenominator * dur.fNumerator));
}

bool rational::operator ==(const rational &dur) const
{
    // a/b < c/d if and only if a * d < b * c.
    return ((fNumerator * dur.fDenominator) == (fDenominator * dur.fNumerator));
}

bool rational::operator >(double num) const
{	
	return (toDouble() > num);
}

bool rational::operator >= (double num) const
{
	return (toDouble() >= num);
}

bool rational::operator <(double num) const
{
	return (toDouble() < num);
}

bool rational::operator <=(double num) const
{
	return (toDouble() <= num);
}

bool rational::operator ==(double num) const
{
	return (toDouble() == num);
}

// gcd(a, b) calculates the gcd of a and b using Euclid's algorithm.
long int rational::gcd(long int a1, long int b1)
{
    long int r;

    long int a = ::abs(a1);
    long int b = ::abs(b1);

    if (!(a == 0) || (b == 0)){
        while (b > 0){
            r = a % b;
            a = b;
            b = r;
        }
        return a;
    }
    return 1;
}

void rational::rationalise()
{
    long int g = gcd(fNumerator, fDenominator);
    fNumerator /= g;
    fDenominator /= g;
    if (fNumerator == 0) fDenominator = 1;
}

double rational::toDouble() const
{
    return (fDenominator != 0) ? ((double)fNumerator/(double)fDenominator) : 0;
}

float rational::toFloat() const
{
    return (fDenominator != 0) ? ((float)fNumerator/(float)fDenominator) : 0;
}

/*
 * Copyright (c) 2003-2014, John Wiegley.  All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are
 * met:
 *
 * - Redistributions of source code must retain the above copyright
 *   notice, this list of conditions and the following disclaimer.
 *
 * - Redistributions in binary form must reproduce the above copyright
 *   notice, this list of conditions and the following disclaimer in the
 *   documentation and/or other materials provided with the distribution.
 *
 * - Neither the name of New Artisans LLC nor the names of its
 *   contributors may be used to endorse or promote products derived from
 *   this software without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 * "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 * LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
 * A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
 * OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
 * SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
 * LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
 * DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
 * THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
 * OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

#include <system.hh>

#include "journal.h"
#include "reader.h"
#include "context.h"
#include "amount.h"
#include "commodity.h"
#include "pool.h"
#include "xact.h"
#include "post.h"
#include "account.h"
#include "option.h"

namespace ledger {

journal_t::journal_t() : master(new account_t)
{
  TRACE_CTOR(journal_t, "");
}

journal_t::~journal_t()
{
  TRACE_DTOR(journal_t);

  // Don't bother unhooking each xact's posts from the accounts they refer to,
  // because all accounts are about to be deleted.
  foreach (xact_t * xact, xacts)
    checked_delete(xact);

  foreach (auto_xact_t * xact, auto_xacts)
    checked_delete(xact);

  foreach (period_xact_t * xact, period_xacts)
    checked_delete(xact);

  checked_delete(master);
}

bool journal_t::remove_xact(xact_t * xact)
{
  bool found = false;
  xacts_list::iterator i;
  for (i = xacts.begin(); i != xacts.end(); i++)
    if (*i == xact) {
      found = true;
      break;
    }
  if (! found)
    return false;

  xacts.erase(i);
  xact->journal = NULL;

  return true;
}

bool journal_t::has_xdata()
{
  foreach (xact_t * xact, xacts)
    if (xact->has_xdata())
      return true;

  foreach (auto_xact_t * xact, auto_xacts)
    if (xact->has_xdata())
      return true;

  foreach (period_xact_t * xact, period_xacts)
    if (xact->has_xdata())
      return true;

  if (master->has_xdata() || master->children_with_xdata())
    return true;

  return false;
}

void journal_t::clear_xdata()
{
  foreach (xact_t * xact, xacts)
    if (! xact->has_flags(ITEM_TEMP))
      xact->clear_xdata();

  foreach (auto_xact_t * xact, auto_xacts)
    if (! xact->has_flags(ITEM_TEMP))
      xact->clear_xdata();

  foreach (period_xact_t * xact, period_xacts)
    if (! xact->has_flags(ITEM_TEMP))
      xact->clear_xdata();

  master->clear_xdata();
}

bool journal_t::valid() const
{
  if (! master->valid()) {
    DEBUG("ledger.validate", "journal_t: master not valid");
    return false;
  }

  foreach (const xact_t * xact, xacts)
    if (! xact->valid()) {
      DEBUG("ledger.validate", "journal_t: xact not valid");
      return false;
    }

  return true;
}

value_t journal_t::fn_account(call_scope_t& args)
{
  if (args[0].is_string())
    return scope_value(master->find_account(args.get<string>(0), false));
  else if (args[0].is_mask())
    return scope_value(master->find_account_re(args.get<mask_t>(0).str()));
  else
    return NULL_VALUE;
}

expr_t::ptr_op_t journal_t::lookup(const symbol_t::kind_t kind,
                                   const string& name)
{
  const char * p = name.c_str();

  switch (kind) {
  case symbol_t::FUNCTION:
    switch (*p) {
    case 'a':
      if (is_eq(p, "account"))
        return MAKE_FUNCTOR(journal_t::fn_account);
      break;

    default:
      break;
    }
    break;

  default:
    break;
  }

  return symbol_scope_t::lookup(kind, name);
}

} // namespace ledger

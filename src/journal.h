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

/**
 * @addtogroup data
 */

/**
 * @file   journal.h
 * @author John Wiegley
 *
 * @ingroup data
 */
#ifndef _JOURNAL_H
#define _JOURNAL_H

#include "utils.h"
#include "times.h"
#include "mask.h"
#include "expr.h"
#include "scope.h"

namespace ledger {

class xact_base_t;
class xact_t;
class auto_xact_t;
class period_xact_t;
class post_t;
class account_t;

typedef std::list<xact_t *>        xacts_list;
typedef std::list<auto_xact_t *>   auto_xacts_list;
typedef std::list<period_xact_t *> period_xacts_list;

class journal_t : public symbol_scope_t
{
public:
  class reader_t;

  struct fileinfo_t
  {
    optional<path> filename;
    uintmax_t      size;
    datetime_t     modtime;
    bool           from_stream;

    fileinfo_t() : size(0), from_stream(true) {
      TRACE_CTOR(journal_t::fileinfo_t, "");
    }
    fileinfo_t(const path& _filename)
      : filename(_filename), from_stream(false) {
      size    = file_size(*filename);
      modtime = posix_time::from_time_t(last_write_time(*filename));
      TRACE_CTOR(journal_t::fileinfo_t, "const path&");
    }
    fileinfo_t(const fileinfo_t& info)
      : filename(info.filename), size(info.size),
        modtime(info.modtime), from_stream(info.from_stream)
    {
      TRACE_CTOR(journal_t::fileinfo_t, "copy");
    }
    ~fileinfo_t() throw() {
      TRACE_DTOR(journal_t::fileinfo_t);
    }

#if HAVE_BOOST_SERIALIZATION
  private:
    /** Serialization. */

    friend class boost::serialization::access;

    template<class Archive>
    void serialize(Archive& ar, const unsigned int /* version */) {
      ar & filename;
      ar & size;
      ar & modtime;
      ar & from_stream;
    }
#endif // HAVE_BOOST_SERIALIZATION
  };

  account_t *           master;
  xacts_list            xacts;
  auto_xacts_list       auto_xacts;
  period_xacts_list     period_xacts;
  std::list<fileinfo_t> sources;
  optional<expr_t>      value_expr;

  // Reader used to read the initial data in.
  // jww (2014-05-08): This should be removed!
  unique_ptr<reader_t> reader;

  journal_t();
  ~journal_t();

  std::list<fileinfo_t>::iterator sources_begin() {
    return sources.begin();
  }
  std::list<fileinfo_t>::iterator sources_end() {
    return sources.end();
  }

  bool remove_xact(xact_t * xact);

  bool has_xdata();
  void clear_xdata();

  bool valid() const;

private:
#if HAVE_BOOST_SERIALIZATION
private:
  /** Serialization. */

  friend class boost::serialization::access;

  template<class Archive>
  void serialize(Archive& ar, const unsigned int /* version */) {
    ar & master;
    ar & bucket;
    ar & xacts;
    ar & auto_xacts;
    ar & period_xacts;
    ar & sources;
  }
#endif // HAVE_BOOST_SERIALIZATION

public:
  virtual string description() {
    return _("journal");
  }

  virtual expr_t::ptr_op_t lookup(const symbol_t::kind_t kind,
                                  const string& name);

  value_t fn_account(call_scope_t& scope);
};

} // namespace ledger

#endif // _JOURNAL_H

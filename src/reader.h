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
 * @file   reader.h
 * @author John Wiegley
 *
 * @ingroup data
 */
#ifndef _READER_H
#define _READER_H

#include "commodity.h"
#include "journal.h"
#include "context.h"
#include "option.h"

namespace ledger {

typedef std::pair<mask_t, string>      payee_mapping_t;
typedef std::list<payee_mapping_t>     payee_mappings_t;
typedef std::pair<mask_t, account_t *> account_mapping_t;
typedef std::list<account_mapping_t>   account_mappings_t;
typedef std::map<string, account_t *>  accounts_map;
typedef std::map<string, xact_t *>     checksum_map_t;

typedef std::multimap<string, expr_t::check_expr_pair> tag_check_exprs_map;

class journal_t::reader_t : public symbol_scope_t
{
public:
  journal_t&            journal;
  account_t *           bucket;
  std::set<string>      known_payees;
  std::set<string>      known_tags;
  bool                  fixed_accounts;
  bool                  fixed_payees;
  bool                  fixed_commodities;
  bool                  fixed_metadata;
  bool                  was_loaded;
  bool                  force_checking;
  bool                  check_payees;
  bool                  day_break;
  bool                  recursive_aliases;
  bool                  no_aliases;
  payee_mappings_t      payee_mappings;
  account_mappings_t    account_mappings;
  accounts_map          account_aliases;
  account_mappings_t    payees_for_unknown_accounts;
  checksum_map_t        checksum_map;
  tag_check_exprs_map   tag_check_exprs;
  parse_context_stack_t parsing_context;

  enum checking_style_t {
    CHECK_PERMISSIVE,
    CHECK_NORMAL,
    CHECK_WARNING,
    CHECK_ERROR
  } checking_style;

  reader_t(journal_t& journal);
#if 0
  reader_t(const path& pathname);
  reader_t(const string& str);
#endif

  void initialize();

  account_t * register_account(const string& name, post_t * post,
                               account_t * master = NULL);
  string      register_payee(const string& name, xact_t * xact);
  void        register_commodity(commodity_t& comm,
                                 variant<int, xact_t *, post_t *> context);
  void        register_metadata(const string& key, const value_t& value,
                                variant<int, xact_t *, post_t *> context);

  account_t * expand_aliases(string name, account_t * master_account);

  bool add_xact(xact_t * xact);
  void extend_xact(xact_base_t * xact);

  std::size_t read_journal(const path& pathname);
  std::size_t read_journal_from_string(const string& data);
  std::size_t read_journal_files();

private:
  std::size_t do_read();
  std::size_t read_data(const string& master_account = "");
  std::size_t read_textual();

public:
  virtual string description() {
    return _("journal reader");
  }

  option_t<journal_t::reader_t> * lookup_option(const char * p);

  expr_t::ptr_op_t lookup(const symbol_t::kind_t kind, const string& name);

  void report_options(std::ostream& out)
  {
    HANDLER(cache_).report(out);
    HANDLER(check_payees).report(out);
    HANDLER(day_break).report(out);
    HANDLER(decimal_comma).report(out);
    HANDLER(time_colon).report(out);
    HANDLER(file_).report(out);
    HANDLER(input_date_format_).report(out);
    HANDLER(explicit).report(out);
    HANDLER(master_account_).report(out);
    HANDLER(pedantic).report(out);
    HANDLER(permissive).report(out);
    HANDLER(price_db_).report(out);
    HANDLER(price_exp_).report(out);
    HANDLER(recursive_aliases).report(out);
    HANDLER(no_aliases).report(out);
    HANDLER(strict).report(out);
    HANDLER(value_expr_).report(out);
  }

  /**
   * Option handlers
   */

  OPTION(journal_t::reader_t, cache_);
  OPTION(journal_t::reader_t, check_payees);
  OPTION(journal_t::reader_t, day_break);

  OPTION_(journal_t::reader_t, decimal_comma, DO() {
      commodity_t::decimal_comma_by_default = true;
    });

  OPTION_(journal_t::reader_t, time_colon, DO() {
      commodity_t::time_colon_by_default = true;
    });

  OPTION__
  (journal_t::reader_t, price_exp_, // -Z
   CTOR(journal_t::reader_t, price_exp_) { value = "24"; });

  OPTION__
  (journal_t::reader_t, file_, // -f
   std::list<path> data_files;
   CTOR(journal_t::reader_t, file_) {}
   DO_(str) {
     data_files.push_back(str);
   });

  OPTION_(journal_t::reader_t, input_date_format_, DO_(str) {
      // This changes static variables inside times.h, which affects the
      // basic date parser.
      set_input_date_format(str.c_str());
    });

  OPTION(journal_t::reader_t, explicit);
  OPTION(journal_t::reader_t, master_account_);
  OPTION(journal_t::reader_t, pedantic);
  OPTION(journal_t::reader_t, permissive);
  OPTION(journal_t::reader_t, price_db_);
  OPTION(journal_t::reader_t, strict);
  OPTION(journal_t::reader_t, value_expr_);
  OPTION(journal_t::reader_t, recursive_aliases);
  OPTION(journal_t::reader_t, no_aliases);
};

} // namespace ledger

#endif // _JOURNAL_H

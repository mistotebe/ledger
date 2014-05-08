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

#include "reader.h"
#include "account.h"
#include "xact.h"
#include "post.h"

namespace ledger {

journal_t::reader_t::reader_t(journal_t& _journal) : journal(_journal)
{
  initialize();
  TRACE_CTOR(journal_t::reader_t, "journal_t&");
}

#if 0
journal_t::reader_t::reader_t(const path& pathname)
{
  initialize();
  read(pathname);
  TRACE_CTOR(journal_t::reader_t, "path");
}

journal_t::reader_t::reader_t(const string& str)
{
  initialize();
  read(str);
  TRACE_CTOR(journal_t::reader_t, "string");
}
#endif

void journal_t::reader_t::initialize()
{
  bucket            = NULL;
  fixed_accounts    = false;
  fixed_payees      = false;
  fixed_commodities = false;
  fixed_metadata    = false;
  was_loaded        = false;
  force_checking    = false;
  check_payees      = false;
  day_break         = false;
  checking_style    = CHECK_NORMAL;
  recursive_aliases = false;
  no_aliases        = false;
}

account_t * journal_t::reader_t::register_account(
  const string& name, post_t * post, account_t * master_account)
{
  // If there are any account aliases, substitute before creating an account
  // object.
  account_t * result = expand_aliases(name, master_account);

  // Create the account object and associate it with the journal; this
  // is registering the account.
  if (! result)
    result = master_account->find_account(name);

  // If the account name being registered is "Unknown", check whether
  // the payee indicates an account that should be used.
  if (result->name == _("Unknown")) {
    foreach (account_mapping_t& value, payees_for_unknown_accounts) {
      if (post && value.first.match(post->xact->payee)) {
        result = value.second;
        break;
      }
    }
  }

  // Now that we have an account, make certain that the account is
  // "known", if the user has requested validation of that fact.
  if (checking_style == CHECK_WARNING || checking_style == CHECK_ERROR) {
    if (! result->has_flags(ACCOUNT_KNOWN)) {
      if (! post) {
        if (force_checking)
          fixed_accounts = true;
        result->add_flags(ACCOUNT_KNOWN);
      }
      else if (! fixed_accounts && post->_state != item_t::UNCLEARED) {
        result->add_flags(ACCOUNT_KNOWN);
      }
      else if (checking_style == CHECK_WARNING) {
        // jww (2014-05-08): This will throw an exception if there is no
        // current parsing context.
        parsing_context.get_current().warning(
          _f("Unknown account '%1%'") % result->fullname());
      }
      else if (checking_style == CHECK_ERROR) {
        throw_(parse_error, _f("Unknown account '%1%'") % result->fullname());
      }
    }
  }
  return result;
}

account_t * journal_t::reader_t::expand_aliases(
  string name, account_t * master_account)
{
  // Aliases are expanded recursively, so if both alias Foo=Bar:Foo and
  // alias Bar=Baaz:Bar are in effect, first Foo will be expanded to
  // Bar:Foo, then Bar:Foo will be expanded to Baaz:Bar:Foo.
  //
  // The expansion loop keeps a list of already expanded names in order
  // to prevent infinite excursion. Each alias may only be expanded at
  // most once.
  account_t * result = NULL;

  if (no_aliases)
    return result;

  bool keep_expanding = true;
  std::list<string> already_seen;
  // Loop until no expansion can be found.
  do {
    if (account_aliases.size() > 0) {
      accounts_map::const_iterator i = account_aliases.find(name);
      if (i != account_aliases.end()) {
        if (std::find(already_seen.begin(), already_seen.end(), name)
            != already_seen.end()) {
          throw_(std::runtime_error,
                 _f("Infinite recursion on alias expansion for %1%") % name);
        }
        // There is an alias for the full account name, including
        // colons.
        already_seen.push_back(name);
        result = (*i).second;
        name = result->fullname();
      } else {
        // Only check the very first account for alias expansion, in
        // case that can be expanded successfully.
        size_t colon = name.find(':');
        if (colon != string::npos) {
          string first_account_name = name.substr(0, colon);
          accounts_map::const_iterator j =
            account_aliases.find(first_account_name);
          if (j != account_aliases.end()) {
            if(std::find(already_seen.begin(), already_seen.end(),
                         first_account_name) != already_seen.end()) {
              throw_(std::runtime_error,
                     _f("Infinite recursion on alias expansion for %1%")
                     % first_account_name);
            }
            already_seen.push_back(first_account_name);
            result = master_account->find_account(
              (*j).second->fullname() + name.substr(colon));
            name = result->fullname();
          } else {
            keep_expanding = false;
          }
        } else {
          keep_expanding = false;
        }
      }
    } else {
      keep_expanding = false;
    }
  }
  while (keep_expanding && recursive_aliases);

  return result;
}

string journal_t::reader_t::register_payee(const string& name, xact_t * xact)
{
  string payee;

  if (check_payees &&
      (checking_style == CHECK_WARNING || checking_style == CHECK_ERROR)) {
    std::set<string>::iterator i = known_payees.find(name);

    if (i == known_payees.end()) {
      if (! xact) {
        if (force_checking)
          fixed_payees = true;
        known_payees.insert(name);
      }
      else if (! fixed_payees && xact->_state != item_t::UNCLEARED) {
        known_payees.insert(name);
      }
      else if (checking_style == CHECK_WARNING) {
        parsing_context.get_current().warning(_f("Unknown payee '%1%'") % name);
      }
      else if (checking_style == CHECK_ERROR) {
        throw_(parse_error, _f("Unknown payee '%1%'") % name);
      }
    }
  }

  foreach (payee_mapping_t& value, payee_mappings) {
    if (value.first.match(name)) {
      payee = value.second;
      break;
    }
  }

  return payee.empty() ? name : payee;
}

void journal_t::reader_t::register_commodity(commodity_t& comm,
                                   variant<int, xact_t *, post_t *> context)
{
  if (checking_style == CHECK_WARNING || checking_style == CHECK_ERROR) {
    if (! comm.has_flags(COMMODITY_KNOWN)) {
      if (context.which() == 0) {
        if (force_checking)
          fixed_commodities = true;
        comm.add_flags(COMMODITY_KNOWN);
      }
      else if (! fixed_commodities &&
               ((context.which() == 1 &&
                 boost::get<xact_t *>(context)->_state != item_t::UNCLEARED) ||
                (context.which() == 2 &&
                 boost::get<post_t *>(context)->_state != item_t::UNCLEARED))) {
        comm.add_flags(COMMODITY_KNOWN);
      }
      else if (checking_style == CHECK_WARNING) {
        parsing_context.get_current().warning(
          _f("Unknown commodity '%1%'") % comm);
      }
      else if (checking_style == CHECK_ERROR) {
        throw_(parse_error, _f("Unknown commodity '%1%'") % comm);
      }
    }
  }
}

void journal_t::reader_t::register_metadata(const string& key, const value_t& value,
                                  variant<int, xact_t *, post_t *> context)
{
  if (checking_style == CHECK_WARNING || checking_style == CHECK_ERROR) {
    std::set<string>::iterator i = known_tags.find(key);

    if (i == known_tags.end()) {
      if (context.which() == 0) {
        if (force_checking)
          fixed_metadata = true;
        known_tags.insert(key);
      }
      else if (! fixed_metadata &&
               ((context.which() == 1 &&
                 boost::get<xact_t *>(context)->_state != item_t::UNCLEARED) ||
                (context.which() == 2 &&
                 boost::get<post_t *>(context)->_state != item_t::UNCLEARED))) {
        known_tags.insert(key);
      }
      else if (checking_style == CHECK_WARNING) {
        parsing_context.get_current().warning(
          _f("Unknown metadata tag '%1%'") % key);
      }
      else if (checking_style == CHECK_ERROR) {
        throw_(parse_error, _f("Unknown metadata tag '%1%'") % key);
      }
    }
  }

  if (! value.is_null()) {
    std::pair<tag_check_exprs_map::iterator,
              tag_check_exprs_map::iterator> range =
      tag_check_exprs.equal_range(key);

    for (tag_check_exprs_map::iterator i = range.first;
         i != range.second;
         ++i) {
      bind_scope_t bound_scope
        (*this, (context.which() == 1 ?
                 static_cast<scope_t&>(*boost::get<xact_t *>(context)) :
                 static_cast<scope_t&>(*boost::get<post_t *>(context))));
      value_scope_t val_scope(bound_scope, value);

      if (! (*i).second.first.calc(val_scope).to_boolean()) {
        if ((*i).second.second == expr_t::EXPR_ASSERTION)
          throw_(parse_error,
                 _f("Metadata assertion failed for (%1%: %2%): %3%")
                 % key % value % (*i).second.first);
        else
          parsing_context.get_current().warning(
            _f("Metadata check failed for (%1%: %2%): %3%")
              % key % value % (*i).second.first);
      }
    }
  }
}

bool lt_posting_account(post_t * left, post_t * right) {
  return left->account < right->account;
}

bool is_equivalent_posting(post_t * left, post_t * right)
{
  if (left->account != right->account)
    return false;

  if (left->amount != right->amount)
    return false;

  return true;
}

namespace {
  void check_all_metadata(
    journal_t::reader_t& reader, variant<int, xact_t *, post_t *> context)
  {
    xact_t * xact = context.which() == 1 ? boost::get<xact_t *>(context) : NULL;
    post_t * post = context.which() == 2 ? boost::get<post_t *>(context) : NULL;

    if ((xact || post) && xact ? xact->metadata : post->metadata) {
      foreach (const item_t::string_map::value_type& pair,
               xact ? *xact->metadata : *post->metadata) {
        const string& key(pair.first);

        if (optional<value_t> value = pair.second.first)
          reader.register_metadata(key, *value, context);
        else
          reader.register_metadata(key, NULL_VALUE, context);
      }
    }
  }
}

bool journal_t::reader_t::add_xact(xact_t * xact)
{
  xact->journal = &journal;

  if (! xact->finalize()) {
    xact->journal = NULL;
    return false;
  }

  extend_xact(xact);
  check_all_metadata(*this, xact);

  foreach (post_t * post, xact->posts) {
    extend_post(*post, journal);
    check_all_metadata(*this, post);
  }

  // If a transaction with this UUID has already been seen, simply do
  // not add this one to the journal.  However, all automated checks
  // will have been performed by extend_xact, so asserts can still be
  // applied to it.
  if (optional<value_t> ref = xact->get_tag(_("UUID"))) {
    std::string uuid = ref->to_string();
    std::pair<checksum_map_t::iterator, bool> result
      = checksum_map.insert(checksum_map_t::value_type(uuid, xact));
    if (! result.second) {
      // This UUID has been seen before; apply any postings which the
      // earlier version may have deferred.
      foreach (post_t * post, xact->posts) {
        account_t * acct = post->account;
        if (acct->deferred_posts) {
          auto i = acct->deferred_posts->find(uuid);
          if (i != acct->deferred_posts->end()) {
            for (post_t * rpost : (*i).second)
              if (acct == rpost->account)
                acct->add_post(rpost);
            acct->deferred_posts->erase(i);
          }
        }
      }

      xact_t * other = (*result.first).second;

      // Copy the two lists of postings (which should be relatively
      // short), and make sure that the intersection is the empty set
      // (i.e., that they are the same list).
      std::vector<post_t *> this_posts(xact->posts.begin(),
                                       xact->posts.end());
      std::sort(this_posts.begin(), this_posts.end(), lt_posting_account);
      std::vector<post_t *> other_posts(other->posts.begin(),
                                        other->posts.end());
      std::sort(other_posts.begin(), other_posts.end(), lt_posting_account);
      bool match = std::equal(this_posts.begin(), this_posts.end(),
                              other_posts.begin(), is_equivalent_posting);

      if (! match || this_posts.size() != other_posts.size()) {
        add_error_context(_("While comparing this previously seen transaction:"));
        add_error_context(source_context(other->pos->pathname,
                                         other->pos->beg_pos,
                                         other->pos->end_pos, "> "));
        add_error_context(_("to this later transaction:"));
        add_error_context(source_context(xact->pos->pathname,
                                         xact->pos->beg_pos,
                                         xact->pos->end_pos, "> "));
        throw_(std::runtime_error,
               _f("Transactions with the same UUID must have equivalent postings"));
      }

      xact->journal = NULL;
      return false;
    }
  }

  journal.xacts.push_back(xact);

  return true;
}

void journal_t::reader_t::extend_xact(xact_base_t * xact)
{
  foreach (auto_xact_t * auto_xact, journal.auto_xacts)
    auto_xact->extend_xact(*xact, parsing_context.get_current());
}

std::size_t journal_t::reader_t::do_read()
{
  std::size_t count = 0;
  try {
    count = read_textual();
    if (count > 0) {
      if (! parsing_context.get_current().pathname.empty())
        journal.sources.push_back(
          fileinfo_t(parsing_context.get_current().pathname));
      else
        journal.sources.push_back(fileinfo_t());
    }
  }
  catch (...) {
    journal.clear_xdata();
    throw;
  }

  // xdata may have been set for some accounts and transaction due to the use
  // of balance assertions or other calculations performed in valexpr-based
  // posting amounts.
  journal.clear_xdata();

  return count;
}

std::size_t journal_t::reader_t::read_data(const string& master_account)
{
  bool populated_data_files = false;

  if (HANDLER(file_).data_files.empty()) {
    path file;
    if (const char * home_var = std::getenv("HOME"))
      file = path(home_var) / ".ledger";

    if (! file.empty() && exists(file))
      HANDLER(file_).data_files.push_back(file);
    else
      throw_(parse_error, "No journal file was specified (please use -f)");

    populated_data_files = true;
  }

  std::size_t xact_count = 0;

  optional<path> price_db_path;
  if (HANDLED(price_db_)){
    price_db_path = resolve_path(HANDLER(price_db_).str());
    if (!exists(price_db_path.get())){
      throw_(parse_error, _f("Could not find specified price-db file %1%") % price_db_path);
    }
  } else {
    if (const char * home_var = std::getenv("HOME")){
      price_db_path = (path(home_var) / ".pricedb");
    } else {
      price_db_path = ("./.ledgerrc");
    }
  }

  if (HANDLED(day_break))
    day_break = true;

  if (HANDLED(recursive_aliases))
    recursive_aliases = true;
  if (HANDLED(no_aliases))
    no_aliases = true;

  if (HANDLED(explicit))
    force_checking = true;
  if (HANDLED(check_payees))
    check_payees = true;

  if (HANDLED(permissive))
    checking_style = CHECK_PERMISSIVE;
  else if (HANDLED(pedantic))
    checking_style = CHECK_ERROR;
  else if (HANDLED(strict))
    checking_style = CHECK_WARNING;

  if (HANDLED(value_expr_))
    journal.value_expr = HANDLER(value_expr_).str();

#if HAVE_BOOST_SERIALIZATION
  optional<archive_t> cache;
  if (HANDLED(cache_) && master_account.empty())
    cache = archive_t(HANDLED(cache_).str());

  if (! (cache &&
         cache->should_load(HANDLER(file_).data_files) &&
         cache->load(journal))) {
#endif // HAVE_BOOST_SERIALIZATION
    if (price_db_path) {
      if (exists(*price_db_path)) {
        parsing_context.push(*price_db_path);
        try {
          if (do_read() > 0)
            throw_(parse_error, _("Transactions not allowed in price history file"));
        }
        catch (...) {
          parsing_context.pop();
          throw;
        }
        parsing_context.pop();
      }
    }

    foreach (const path& pathname, HANDLER(file_).data_files) {
      if (pathname == "-" || pathname == "/dev/stdin") {
        // To avoid problems with stdin and pipes, etc., we read the entire
        // file in beforehand into a memory buffer, and then parcel it out
        // from there.
        std::ostringstream buffer;

        while (std::cin.good() && ! std::cin.eof()) {
          char line[8192];
          std::cin.read(line, 8192);
          std::streamsize count = std::cin.gcount();
          buffer.write(line, count);
        }
        buffer.flush();

        shared_ptr<std::istream> stream(new std::istringstream(buffer.str()));
        parsing_context.push(stream);
      } else {
        parsing_context.push(pathname);
      }

      try {
        xact_count += do_read();
      }
      catch (...) {
        parsing_context.pop();
        throw;
      }
      parsing_context.pop();
    }

    DEBUG("ledger.read", "xact_count [" << xact_count
          << "] == journal->xacts.size() [" << journal.xacts.size() << "]");
    assert(xact_count == journal.xacts.size());

#if HAVE_BOOST_SERIALIZATION
    if (cache && cache->should_save(journal))
      cache->save(journal);
  }
#endif // HAVE_BOOST_SERIALIZATION

  if (populated_data_files)
    HANDLER(file_).data_files.clear();

  VERIFY(journal.valid());

  return xact_count;
}

std::size_t journal_t::reader_t::read_journal_files()
{
  INFO_START(journal, "Read journal file");

  string master_account;
  if (HANDLED(master_account_))
    master_account = HANDLER(master_account_).str();

  std::size_t count = read_data(master_account);

  INFO_FINISH(journal);

#if DEBUG_ON
  INFO("Found " << count << " transactions");
#endif

  return count;
}

std::size_t journal_t::reader_t::read_journal(const path& pathname)
{
  HANDLER(file_).data_files.clear();
  HANDLER(file_).data_files.push_back(pathname);

  return read_journal_files();
}

std::size_t journal_t::reader_t::read_journal_from_string(const string& data)
{
  shared_ptr<std::istream> stream(new std::istringstream(data));
  parsing_context.push(stream);

  std::size_t count = 0;
  try {
    count = do_read();
  }
  catch (...) {
    parsing_context.pop();
    throw;
  }
  parsing_context.pop();

  return count;
}

#if 0
value_t journal_t::reader_t::fn_account(call_scope_t& args)
{
  // jww (2014-05-08): We no longer support fully relative account name
  // references despite the use of master accounts.  This needs to be
  // restored.
  if (args[0].is_string())
    return scope_value(journal.master->find_account(args.get<string>(0), false));
  else if (args[0].is_mask())
    return scope_value(journal.master->find_account_re(args.get<mask_t>(0).str()));
  else
    return NULL_VALUE;
}
#endif

option_t<journal_t::reader_t> *
journal_t::reader_t::lookup_option(const char * p)
{
  switch (*p) {
  case 'Z':
    OPT_CH(price_exp_);
    break;
  case 'c':
    OPT(cache_);
    else OPT(check_payees);
    break;
  case 'd':
    OPT(decimal_comma);
    else OPT(day_break);
    break;
  case 'e':
    OPT(explicit);
    break;
  case 'f':
    OPT_(file_); // -f
    break;
  case 'i':
    OPT(input_date_format_);
    break;
  case 'l':
    OPT_ALT(price_exp_, leeway_);
    break;
  case 'm':
    OPT(master_account_);
    break;
  case 'n':
    OPT(no_aliases);
    break;
  case 'p':
    OPT(price_db_);
    else OPT(price_exp_);
    else OPT(pedantic);
    else OPT(permissive);
    break;
  case 'r':
    OPT(recursive_aliases);
    break;
  case 's':
    OPT(strict);
    break;
  case 't':
    OPT(time_colon);
    break;
  case 'v':
    OPT(value_expr_);
  }
  return NULL;
}

expr_t::ptr_op_t journal_t::reader_t::lookup(const symbol_t::kind_t kind,
                                             const string& name)
{
  const char * p = name.c_str();

  switch (kind) {
  case symbol_t::FUNCTION:
#if 0
    switch (*p) {
    case 'a':
      if (is_eq(p, "account"))
        return MAKE_FUNCTOR(journal_t::reader_t::fn_account);
      break;

    default:
      break;
    }
#endif
    // Check if they are trying to access an option's setting or value.
    if (option_t<journal_t::reader_t> * handler = lookup_option(p))
      return MAKE_OPT_FUNCTOR(journal_t::reader_t, handler);
    break;

  case symbol_t::OPTION:
    if (option_t<journal_t::reader_t> * handler = lookup_option(p))
      return MAKE_OPT_HANDLER(journal_t::reader_t, handler);
    break;

  default:
    break;
  }

  return symbol_scope_t::lookup(kind, name);
}

} // namespace ledger

# FAQ:
#
# There's a default integer primary key column called
# rowid/oid/__rowid__, so you don't have to declare one. If you do
# declare an integer primary key column, it will be an alias to the
# rowid column.
#
# Everything is stored as strings, except primary key integer columns.
#
# Column types define "type affinities", i.e. hints to SQLite about the
# preferred deserialization format. But you can still store a string in
# an integer column and get it back no problem.
#
# Even dates and timestamps are strings. SQLite3 has some special
# functions which allow you to manipulate and compare them as if they
# were dates. But if you always use the same fixed-width format, you can
# also just use lexicographical comparison for simple stuff.
#
# Similarly for JSON values -- stored as strings, but functions are
# available to manipulate them in sophisticated ways.
#
# SQLite3 by default operates in autocommit mode, but the Python
# bindings do not. By default, it issues a BEGIN statement implicitly
# before a Data Modification Language (DML) statement (i.e.
# INSERT/UPDATE/DELETE/REPLACE). So in order for the changes to be
# written to the database, either explicitly call `con.commit()` where
# appropriate, or wrap the transaction with `with con: ...`.
#
# Foreign key constraint only respected if pragma.

# TODO: what about JSON adapters / converters?

import sqlite3
import datetime as dt

# NOTE: All the bells and whistles here!
con = sqlite3.connect(
    # :memory:!
    "foo.db",
    detect_types=sqlite3.PARSE_DECLTYPES | sqlite3.PARSE_COLNAMES,
)
con.row_factory = sqlite3.Row
con.set_trace_callback(print)
con.execute("create table test(d date, ts timestamp)")

today = dt.date.today()
now = dt.datetime.now()

with con:
    con.execute("insert into test(d, ts) values (?, ?)", (today, now))
row = con.execute("select d, ts from test").fetchone()
for k in row.keys():
    print(k, row[k], sep=" : ")

row = con.execute(
    'select current_date as "d [date]", current_timestamp as "ts [timestamp]"'
).fetchone()
for k in row.keys():
    print(k, row[k], sep=" : ")

con.close()

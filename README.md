# SQL

[![Travis-CI Build Status](https://travis-ci.org/rstats-db/SQL.png?branch=master)](https://travis-ci.org/rstats-db/SQL)

This package provides generics useful for generating SQL code. Each generic is accompanied by a method for DBIConnection with a ANSI SQL-99 compliant implementation. SQL-99 was somewhat arbitrarily picked because reasonable documentation was available in the form of [SQL-99 Complete, Really](https://mariadb.com/kb/en/sql-99/) by Peter Gulutzan & Trudy Pelzer. It covers all of the functionality needed in common use.

The goal of the package is to provide generics that generate the most common queries executed from R code. The goal is not to provide a complete parameterisation of SQL-99 queries. DBI backends that talk to SQL backends should provide their own methods, where the database differs from SQL-99. (In most cases this not necessary because SQL only exposes very simple queries.)

## Safety

SQL strives to genereate safe SQL, i.e. SQL that is not vulnerable to injection attacks. It does this by escaping strings and identifiers using `DBI::dbQuoteString()` and `DBI::dbQuoteIdentifier()`. The documentation for each function parameter describes the escaping policy. There are a few parameters (e.g. `fields` in `sqlCreateTable()`) that can not be fully escaped because the generated code is neither an identifier nor a string. These are clearly labelled, and you should avoid passing arbitray user input to them.

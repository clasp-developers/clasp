#! /bin/env python

from mbb import *


a = Aggregate()

mol2ReadAggregateFromFileName(a,"opener.mol2")

print a.asXml().asString()

mol2WriteAggregateToFileName(a,"openerOut.mol2")

#! /usr/bin/python

################################################################################
# A small quick hack script that re-orders and summarizes group definitions.
#
# It uses quite specific logic to get a new grouping but this might serve as
# an example for similar tools.
################################################################################

################################################################################
# 1st parameter = trace to read
################################################################################

from otf import *
import sys



# dict for new groups
newindex= 0

number_def_function= 0
number_def_group= 0
max_def_group= 0


token_to_name= {}
token_to_shortname= {}
token_to_count= {}

token_to_newtoken= {}


### stage 1 handlers ###########################################################

def handleDefFunction_Stage_1( fha, stream, func, name, group, source, kvlist ):
	
#	print ( ' handleDefFunction: \"%s\", s%u, t%u, nm\"%s\", g%u, src%u' \
#		% (fha,stream, func,name,group,source) )

	global number_def_function
	number_def_function += 1

	token_to_count[ group ] += 1


#	# find first part of source file name
	shortname= "empty"
	a= 0
	b= 0

	a= name.find( "[{" )
	if -1 != a:
		b= name.find( "}", a+2 )
		if -1 != b :

			c= name.find( "_", a+2, b )
			if -1 != c :
				b= c

			c= name.find( ".", a+2, b )
			if -1 != c :
				b= c

			c= name.find( " ", a+2, b )
			if -1 != c :
				b= c

			shortname= name[a+2:b]
	token_to_shortname[ group ]= shortname

	return OTF_RETURN_OK


def handleDefFunctionGroup_Stage_1( fha, stream, group, name, kvlist ):
	
#	print ( ' handleDefFunctionGroup: \"%s\", s%u, t%u, nm\"%s\"' \
#		% (fha,stream,group,name) )

	global number_def_group
	number_def_group += 1

	global max_def_group
	max_def_group= max_def_group if ( max_def_group > group ) else group

	token_to_name[ group ]= name

	global token_to_count
	token_to_count[ group ]= 0

	return OTF_RETURN_OK


### stage 2 handlers ###########################################################


def handleDefFunction_Stage_2( fha, stream, func, name, group, source, kvlist ):
	
	OTF_Writer_writeDefFunction( fha, stream, func, name, token_to_newtoken[ group ], source )

	return OTF_RETURN_OK


def handleDefFunctionGroup_Stage_2( fha, stream, group, name, kvlist ):

	return OTF_RETURN_OK


### main #######################################################################


if __name__ == '__main__':
	
	manager= OTF_FileManager_open( 100 )

	inname= sys.argv[1]
	print ( "read ", inname )

	outname= OTF_stripFilename( inname ) + "_new"
	print ( "write ", outname )

	reader= OTF_Reader_open( inname, manager )

	handlers= OTF_HandlerArray_open()


	### stage 1 - create group mapping #########################################


	OTF_HandlerArray_setHandler( handlers, handleDefFunction_Stage_1, OTF_DEFFUNCTION_RECORD )
	OTF_HandlerArray_setHandler( handlers, handleDefFunctionGroup_Stage_1, OTF_DEFFUNCTIONGROUP_RECORD )


	num= OTF_Reader_readDefinitions( reader, handlers )

	print ( "stage 1: read ", num, " records" )
	print ( "number of function definitions ", number_def_function )
	print ( "number of group definitions", number_def_group )
	print ( "len( token_to_name ) ", len( token_to_name ) )
	print ( "len( token_to_count ) ", len( token_to_count ) )
	print ( "\n" )

#	print ( "list all:" )
#	for k, c in token_to_count.iteritems() :
#		print ( k, " # ", c, " name ", token_to_name[ k ], " shortname ", token_to_shortname[ k ] )

	### compute mapping token_to_newtoken ######################################

	writer= OTF_Writer_open( outname, 0, manager )
	max_def_group += 1

	shortname_to_count= {}

	print ( "list keep:" )
#	for k, c in token_to_count.iteritems() :
	for k, c in token_to_count.items() :
	
		if c >= 5 :
			print ( "keep ", k, " # ", c, " name ", token_to_name[ k ] )
			OTF_Writer_writeDefFunctionGroup( writer, 0, k,  token_to_name[ k ] )
		else :
			print ( "k= ", k )
			print ( "token_to_shortname[ k ]", token_to_shortname[ k ] )
			shortname_to_count[ token_to_shortname[ k ] ]= 0

	# sum over equal short names
#	for k, c in token_to_count.iteritems() :
	for k, c in token_to_count.items() :
	
		shortname_to_count[ token_to_shortname[ k ] ] += c

#	for k, c in token_to_shortname.iteritems() :
#		print ( "    ", k, " # ", c )
		
		
	shortname_to_newtoken= {}

	print ( "list new:" )
#	for k, c in shortname_to_count.iteritems() :
	for k, c in shortname_to_count.items() :

		if c >= 5 :
			OTF_Writer_writeDefFunctionGroup( writer, 0, max_def_group, k )
			shortname_to_newtoken[ k ]= max_def_group
			print ( "    new token ", max_def_group, ":", k, " # ", c )
			max_def_group += 1

	# define "misc" group
	k= "misc"
	OTF_Writer_writeDefFunctionGroup( writer, 0, max_def_group, k )
	shortname_to_newtoken[ k ]= max_def_group
	print ( "    new token ", max_def_group, ":", k, " # x" )
	max_def_group += 1


	token_to_newtoken= {}

#	for k, v in token_to_shortname.iteritems() :
	for k, v in token_to_shortname.items() :

		if token_to_count[ k ] >= 5 :
		
			token_to_newtoken[ k ]= k
		
		else :

#			if shortname_to_newtoken.has_key( v ) :
			if v in shortname_to_newtoken :

				token_to_newtoken[ k ]= shortname_to_newtoken[ v ];
			else :
				token_to_newtoken[ k ]= shortname_to_newtoken[ "misc" ];
			

#	print ( "\n mapping \n\n" )
#	for t, n in token_to_newtoken.iteritems() :
#		print ( "   ", t, " --> ", n )
	

	### stage 2 - write new def file ###########################################

	# set copy handlers to all but FunctionDefs and FunctionGroupDefs
	OTF_HandlerArray_getCopyHandler( handlers, writer )
	OTF_HandlerArray_setHandler( handlers, handleDefFunction_Stage_2, OTF_DEFFUNCTION_RECORD )
	OTF_HandlerArray_setHandler( handlers, handleDefFunctionGroup_Stage_2, OTF_DEFFUNCTIONGROUP_RECORD )

	OTF_Reader_close( reader )
	reader= OTF_Reader_open( inname, manager )

	num= OTF_Reader_readDefinitions( reader, handlers )

	print ( "stage 2: read ", num, " records" )

	OTF_HandlerArray_close( handlers )
	OTF_Writer_close( writer )
	OTF_Reader_close( reader )
	OTF_FileManager_close( manager )

/**
 * VampirTrace
 * http://www.tu-dresden.de/zih/vampirtrace
 *
 * Copyright (c) 2005-2013, ZIH, TU Dresden, Federal Republic of Germany
 *
 * Copyright (c) 1998-2005, Forschungszentrum Juelich, Juelich Supercomputing
 *                          Centre, Federal Republic of Germany
 *
 * See the file COPYING in the package base directory for details
 **/

#include "vt_unify.h"
#include "vt_unify_defs_recs.h"

#ifdef VT_MPI

//////////////////// struct DefRec_BaseS ////////////////////

VT_MPI_INT
DefRec_BaseS::getPackSize()
{
   VT_MPI_INT buffer_size;

   // dtype + loccpuid + deftoken
   CALL_MPI( MPI_Pack_size( 3, MPI_UNSIGNED, MPI_COMM_WORLD, &buffer_size ) );

   return buffer_size;
}

void
DefRec_BaseS::pack( char *& buffer, const VT_MPI_INT & bufferSize,
                    VT_MPI_INT & bufferPos )
{
   // dtype
   CALL_MPI( MPI_Pack( &dtype, 1, MPI_UNSIGNED, buffer, bufferSize, &bufferPos,
                       MPI_COMM_WORLD ) );

   // loccpuid
   CALL_MPI( MPI_Pack( &loccpuid, 1, MPI_UNSIGNED, buffer, bufferSize,
                       &bufferPos, MPI_COMM_WORLD ) );

   // deftoken
   CALL_MPI( MPI_Pack( &deftoken, 1, MPI_UNSIGNED, buffer, bufferSize,
                       &bufferPos, MPI_COMM_WORLD ) );
}

void
DefRec_BaseS::unpack( char *& buffer, const VT_MPI_INT & bufferSize,
                      VT_MPI_INT & bufferPos )
{
   // dtype
   CALL_MPI( MPI_Unpack( buffer, bufferSize, &bufferPos, &dtype, 1,
                         MPI_UNSIGNED, MPI_COMM_WORLD ) );

   // loccpuid
   CALL_MPI( MPI_Unpack( buffer, bufferSize, &bufferPos, &loccpuid, 1,
                         MPI_UNSIGNED, MPI_COMM_WORLD ) );

   // deftoken
   CALL_MPI( MPI_Unpack( buffer, bufferSize, &bufferPos, &deftoken, 1,
                         MPI_UNSIGNED, MPI_COMM_WORLD ) );
}

//////////////////// struct DefRec_DefCommentS ////////////////////

VT_MPI_INT
DefRec_DefCommentS::getPackSize()
{
   VT_MPI_INT buffer_size = DefRec_BaseS::getPackSize();
   VT_MPI_INT size;

   // type + comment.length()
   //
   CALL_MPI( MPI_Pack_size( 2, MPI_UNSIGNED, MPI_COMM_WORLD, &size ) );
   buffer_size += size;

   // comment
   //
   CALL_MPI( MPI_Pack_size( comment.length() + 1, MPI_CHAR, MPI_COMM_WORLD,
                            &size ) );
   buffer_size += size;

   return buffer_size;
}

void
DefRec_DefCommentS::pack( char *& buffer, const VT_MPI_INT & bufferSize,
                          VT_MPI_INT & bufferPos )
{
   DefRec_BaseS::pack( buffer, bufferSize, bufferPos );

   // type
   CALL_MPI( MPI_Pack( &type, 1, MPI_UNSIGNED, buffer, bufferSize, &bufferPos,
                       MPI_COMM_WORLD ) );

   // comment.length()
   //
   uint32_t comment_length = comment.length();
   CALL_MPI( MPI_Pack( &comment_length, 1, MPI_UNSIGNED, buffer, bufferSize,
                       &bufferPos, MPI_COMM_WORLD ) );

   // comment
   //
   char * c_comment = new char[comment_length+1];
   vt_assert( c_comment );
   strcpy( c_comment, comment.c_str() );
   CALL_MPI( MPI_Pack( c_comment, comment_length + 1, MPI_CHAR, buffer,
                       bufferSize, &bufferPos, MPI_COMM_WORLD ) );
   delete [] c_comment;
}

void
DefRec_DefCommentS::unpack( char *& buffer, const VT_MPI_INT & bufferSize,
                            VT_MPI_INT & bufferPos )
{
   DefRec_BaseS::unpack( buffer, bufferSize, bufferPos );

   // type
   CALL_MPI( MPI_Unpack( buffer, bufferSize, &bufferPos, &type, 1, MPI_UNSIGNED,
                         MPI_COMM_WORLD ) );

   // comment.length()
   //
   uint32_t comment_length;
   CALL_MPI( MPI_Unpack( buffer, bufferSize, &bufferPos, &comment_length, 1,
                         MPI_UNSIGNED, MPI_COMM_WORLD ) );

   // comment
   //
   char * c_comment = new char[comment_length+1];
   vt_assert( c_comment );
   CALL_MPI( MPI_Unpack( buffer, bufferSize, &bufferPos, c_comment,
                         comment_length + 1, MPI_CHAR, MPI_COMM_WORLD ) );
   comment = c_comment;
   delete [] c_comment;
}

//////////////////// struct DefRec_DefCreatorS ////////////////////

VT_MPI_INT
DefRec_DefCreatorS::getPackSize()
{
   VT_MPI_INT buffer_size = DefRec_BaseS::getPackSize();
   VT_MPI_INT size;

   // creator.length()
   //
   uint32_t creator_length = creator.length();
   CALL_MPI( MPI_Pack_size( 1, MPI_UNSIGNED, MPI_COMM_WORLD, &size ) );
   buffer_size += size;

   // creator
   //
   CALL_MPI( MPI_Pack_size( creator_length + 1, MPI_CHAR, MPI_COMM_WORLD,
                            &size ) );
   buffer_size += size;

   return buffer_size;
}

void
DefRec_DefCreatorS::pack( char *& buffer, const VT_MPI_INT & bufferSize,
                          VT_MPI_INT & bufferPos )
{
   DefRec_BaseS::pack( buffer, bufferSize, bufferPos );

   // creator.length()
   //
   uint32_t creator_length = creator.length();
   CALL_MPI( MPI_Pack( &creator_length, 1, MPI_UNSIGNED, buffer, bufferSize,
                       &bufferPos, MPI_COMM_WORLD ) );

   // creator
   //
   char * c_creator = new char[creator_length+1];
   vt_assert( c_creator );
   strcpy( c_creator, creator.c_str() );
   CALL_MPI( MPI_Pack( c_creator, creator_length + 1, MPI_CHAR, buffer,
                       bufferSize, &bufferPos, MPI_COMM_WORLD ) );
   delete [] c_creator;
}

void
DefRec_DefCreatorS::unpack( char *& buffer, const VT_MPI_INT & bufferSize,
                            VT_MPI_INT & bufferPos )
{
   DefRec_BaseS::unpack( buffer, bufferSize, bufferPos );

   // creator.length()
   //
   uint32_t creator_length;
   CALL_MPI( MPI_Unpack( buffer, bufferSize, &bufferPos, &creator_length, 1,
                         MPI_UNSIGNED, MPI_COMM_WORLD ) );

   // creator
   //
   char * c_creator = new char[creator_length+1];
   vt_assert( c_creator );
   CALL_MPI( MPI_Unpack( buffer, bufferSize, &bufferPos, c_creator,
                         creator_length + 1, MPI_CHAR, MPI_COMM_WORLD ) );
   creator = c_creator;
   delete [] c_creator;
}

//////////////////// struct DefRec_DefTimerResolutionS ////////////////////

VT_MPI_INT
DefRec_DefTimerResolutionS::getPackSize()
{
   VT_MPI_INT buffer_size = DefRec_BaseS::getPackSize();
   VT_MPI_INT size;

   // ticksPerSecond
   //
   CALL_MPI( MPI_Pack_size( 1, MPI_LONG_LONG_INT, MPI_COMM_WORLD, &size ) );
   buffer_size += size;

   return buffer_size;
}

void
DefRec_DefTimerResolutionS::pack( char *& buffer, const VT_MPI_INT & bufferSize,
                                  VT_MPI_INT & bufferPos )
{
   DefRec_BaseS::pack( buffer, bufferSize, bufferPos );

   // ticksPerSecond
   CALL_MPI( MPI_Pack( &ticksPerSecond, 1, MPI_LONG_LONG_INT, buffer,
                       bufferSize, &bufferPos, MPI_COMM_WORLD ) );
}

void
DefRec_DefTimerResolutionS::unpack( char *& buffer,
                                    const VT_MPI_INT & bufferSize,
                                    VT_MPI_INT & bufferPos )
{
   DefRec_BaseS::unpack( buffer, bufferSize, bufferPos );

   // ticksPerSecond
   CALL_MPI( MPI_Unpack( buffer, bufferSize, &bufferPos, &ticksPerSecond, 1,
                         MPI_LONG_LONG_INT, MPI_COMM_WORLD ) );
}

//////////////////// struct DefRec_DefTimeRangeS ////////////////////

VT_MPI_INT
DefRec_DefTimeRangeS::getPackSize()
{
   VT_MPI_INT buffer_size = DefRec_BaseS::getPackSize();
   VT_MPI_INT size;

   // minTime + maxTime
   //
   CALL_MPI( MPI_Pack_size( 2, MPI_LONG_LONG_INT, MPI_COMM_WORLD, &size ) );
   buffer_size += size;

   return buffer_size;
}

void
DefRec_DefTimeRangeS::pack( char *& buffer, const VT_MPI_INT & bufferSize,
                            VT_MPI_INT & bufferPos )
{
   DefRec_BaseS::pack( buffer, bufferSize, bufferPos );

   // minTime
   CALL_MPI( MPI_Pack( &minTime, 1, MPI_LONG_LONG_INT, buffer,
                       bufferSize, &bufferPos, MPI_COMM_WORLD ) );

   // maxTime
   CALL_MPI( MPI_Pack( &maxTime, 1, MPI_LONG_LONG_INT, buffer,
                       bufferSize, &bufferPos, MPI_COMM_WORLD ) );
}

void
DefRec_DefTimeRangeS::unpack( char *& buffer, const VT_MPI_INT & bufferSize,
                              VT_MPI_INT & bufferPos )
{
   DefRec_BaseS::unpack( buffer, bufferSize, bufferPos );

   // minTime
   CALL_MPI( MPI_Unpack( buffer, bufferSize, &bufferPos, &minTime, 1,
                         MPI_LONG_LONG_INT, MPI_COMM_WORLD ) );

   // maxTime
   CALL_MPI( MPI_Unpack( buffer, bufferSize, &bufferPos, &maxTime, 1,
                         MPI_LONG_LONG_INT, MPI_COMM_WORLD ) );
}

//////////////////// struct DefRec_DefProcessS ////////////////////

VT_MPI_INT
DefRec_DefProcessS::getPackSize()
{
   VT_MPI_INT buffer_size = DefRec_BaseS::getPackSize();
   VT_MPI_INT size;

   // name.length() + parent
   //
   CALL_MPI( MPI_Pack_size( 2, MPI_UNSIGNED, MPI_COMM_WORLD, &size ) );
   buffer_size += size;

   // name
   //
   CALL_MPI( MPI_Pack_size( name.length() + 1, MPI_CHAR, MPI_COMM_WORLD,
                            &size ) );
   buffer_size += size;

   return buffer_size;
}

void
DefRec_DefProcessS::pack( char *& buffer, const VT_MPI_INT & bufferSize,
                          VT_MPI_INT & bufferPos )
{
   DefRec_BaseS::pack( buffer, bufferSize, bufferPos );

   // name.length()
   //
   uint32_t name_length = name.length();
   CALL_MPI( MPI_Pack( &name_length, 1, MPI_UNSIGNED, buffer, bufferSize,
                       &bufferPos, MPI_COMM_WORLD ) );

   // name
   //
   char * c_name = new char[name_length+1];
   vt_assert( c_name );
   strcpy( c_name, name.c_str() );
   CALL_MPI( MPI_Pack( c_name, name_length + 1, MPI_CHAR, buffer, bufferSize,
                       &bufferPos, MPI_COMM_WORLD ) );
   delete [] c_name;

   // parent
   CALL_MPI( MPI_Pack( &parent, 1, MPI_UNSIGNED, buffer, bufferSize, &bufferPos,
                       MPI_COMM_WORLD ) );
}

void
DefRec_DefProcessS::unpack( char *& buffer, const VT_MPI_INT & bufferSize,
                            VT_MPI_INT & bufferPos )
{
   DefRec_BaseS::unpack( buffer, bufferSize, bufferPos );

   // name.length()
   //
   uint32_t name_length;
   CALL_MPI( MPI_Unpack( buffer, bufferSize, &bufferPos, &name_length, 1,
                         MPI_UNSIGNED, MPI_COMM_WORLD ) );

   // name
   //
   char * c_name = new char[name_length+1];
   vt_assert( c_name );
   CALL_MPI( MPI_Unpack( buffer, bufferSize, &bufferPos, c_name,
                         name_length + 1, MPI_CHAR, MPI_COMM_WORLD ) );
   name = c_name;
   delete [] c_name;

   // parent
   CALL_MPI( MPI_Unpack( buffer, bufferSize, &bufferPos, &parent, 1,
                         MPI_UNSIGNED, MPI_COMM_WORLD ) );
}

//////////////////// struct DefRec_DefProcessGroupS ////////////////////

VT_MPI_INT
DefRec_DefProcessGroupS::getPackSize()
{
   VT_MPI_INT buffer_size = DefRec_BaseS::getPackSize();
   VT_MPI_INT size;

   // type + name.length() + members_hash + nmembers + members
   //
   CALL_MPI( MPI_Pack_size( 4 + nmembers, MPI_UNSIGNED, MPI_COMM_WORLD,
                            &size ) );
   buffer_size += size;

   // name
   //
   CALL_MPI( MPI_Pack_size( name.length() + 1, MPI_CHAR, MPI_COMM_WORLD,
                            &size ) );
   buffer_size += size;

   return buffer_size;
}

void
DefRec_DefProcessGroupS::pack( char *& buffer, const VT_MPI_INT & bufferSize,
                               VT_MPI_INT & bufferPos )
{
   DefRec_BaseS::pack( buffer, bufferSize, bufferPos );

   // type
   CALL_MPI( MPI_Pack( &type, 1, MPI_UNSIGNED, buffer, bufferSize, &bufferPos,
                       MPI_COMM_WORLD ) );

   // name.length()
   //
   uint32_t name_length = name.length();
   CALL_MPI( MPI_Pack( &name_length, 1, MPI_UNSIGNED, buffer, bufferSize,
                       &bufferPos, MPI_COMM_WORLD ) );

   // name
   //
   char * c_name = new char[name_length+1];
   vt_assert( c_name );
   strcpy( c_name, name.c_str() );
   CALL_MPI( MPI_Pack( c_name, name_length + 1, MPI_CHAR, buffer, bufferSize,
                       &bufferPos, MPI_COMM_WORLD ) );
   delete [] c_name;

   // members_hash
   CALL_MPI( MPI_Pack( &members_hash, 1, MPI_UNSIGNED, buffer, bufferSize,
                       &bufferPos, MPI_COMM_WORLD ) );

   // nmembers
   CALL_MPI( MPI_Pack( &nmembers, 1, MPI_UNSIGNED, buffer, bufferSize,
                       &bufferPos, MPI_COMM_WORLD ) );

   // members
   //
   if( nmembers > 0 )
   {
      CALL_MPI( MPI_Pack( members, nmembers, MPI_UNSIGNED, buffer, bufferSize,
                          &bufferPos, MPI_COMM_WORLD ) );
   }
}

void
DefRec_DefProcessGroupS::unpack( char *& buffer, const VT_MPI_INT & bufferSize,
                                 VT_MPI_INT & bufferPos )
{
   DefRec_BaseS::unpack( buffer, bufferSize, bufferPos );

   // type
   CALL_MPI( MPI_Unpack( buffer, bufferSize, &bufferPos, &type, 1, MPI_UNSIGNED,
                         MPI_COMM_WORLD ) );

   // name.length()
   //
   uint32_t name_length;
   CALL_MPI( MPI_Unpack( buffer, bufferSize, &bufferPos, &name_length, 1,
                         MPI_UNSIGNED, MPI_COMM_WORLD ) );

   // name
   //
   char * c_name = new char[name_length+1];
   vt_assert( c_name );
   CALL_MPI( MPI_Unpack( buffer, bufferSize, &bufferPos, c_name,
                         name_length + 1, MPI_CHAR, MPI_COMM_WORLD ) );
   name = c_name;
   delete [] c_name;

   // members_hash
   CALL_MPI( MPI_Unpack( buffer, bufferSize, &bufferPos, &members_hash, 1,
                         MPI_UNSIGNED, MPI_COMM_WORLD ) );

   // nmembers
   CALL_MPI( MPI_Unpack( buffer, bufferSize, &bufferPos, &nmembers, 1,
                         MPI_UNSIGNED, MPI_COMM_WORLD ) );

   // members
   //
   if( nmembers > 0 )
   {
      members = new uint32_t[nmembers];
      vt_assert( members );
      CALL_MPI( MPI_Unpack( buffer, bufferSize, &bufferPos, members,
                            nmembers, MPI_UNSIGNED, MPI_COMM_WORLD ) );
   }
}

/////////////////// struct DefRec_DefProcessGroupAttributesS ///////////////////

VT_MPI_INT
DefRec_DefProcessGroupAttributesS::getPackSize()
{
   VT_MPI_INT buffer_size = DefRec_BaseS::getPackSize();
   VT_MPI_INT size;

   // attributes
   //
   CALL_MPI( MPI_Pack_size( 1, MPI_UNSIGNED, MPI_COMM_WORLD, &size ) );
   buffer_size += size;

   return buffer_size;
}

void
DefRec_DefProcessGroupAttributesS::pack( char *& buffer,
                                         const VT_MPI_INT & bufferSize,
                                         VT_MPI_INT & bufferPos )
{
   DefRec_BaseS::pack( buffer, bufferSize, bufferPos );

   // attributes
   CALL_MPI( MPI_Pack( &attributes, 1, MPI_UNSIGNED, buffer, bufferSize, &bufferPos,
                       MPI_COMM_WORLD ) );
}

void
DefRec_DefProcessGroupAttributesS::unpack( char *& buffer,
                                           const VT_MPI_INT & bufferSize,
                                           VT_MPI_INT & bufferPos )
{
   DefRec_BaseS::unpack( buffer, bufferSize, bufferPos );

   // attributes
   CALL_MPI( MPI_Unpack( buffer, bufferSize, &bufferPos, &attributes, 1,
                         MPI_UNSIGNED, MPI_COMM_WORLD ) );
}

//////////////////// struct DefRec_DefSclFileS ////////////////////

VT_MPI_INT
DefRec_DefSclFileS::getPackSize()
{
   VT_MPI_INT buffer_size = DefRec_BaseS::getPackSize();
   VT_MPI_INT size;

   // filename.length()
   //
   CALL_MPI( MPI_Pack_size( 1, MPI_UNSIGNED, MPI_COMM_WORLD, &size ) );
   buffer_size += size;

   // filename
   //
   CALL_MPI( MPI_Pack_size( filename.length() + 1, MPI_CHAR, MPI_COMM_WORLD,
                            &size ) );
   buffer_size += size;

   return buffer_size;
}

void
DefRec_DefSclFileS::pack( char *& buffer, const VT_MPI_INT & bufferSize,
                          VT_MPI_INT & bufferPos )
{
   DefRec_BaseS::pack( buffer, bufferSize, bufferPos );

   // filename.length()
   //
   uint32_t filename_length = filename.length();
   CALL_MPI( MPI_Pack( &filename_length, 1, MPI_UNSIGNED, buffer, bufferSize,
                       &bufferPos, MPI_COMM_WORLD ) );

   // filename
   //
   char * c_filename = new char[filename_length+1];
   vt_assert( c_filename );
   strcpy( c_filename, filename.c_str() );
   CALL_MPI( MPI_Pack( c_filename, filename_length + 1, MPI_CHAR, buffer,
                       bufferSize, &bufferPos, MPI_COMM_WORLD ) );
   delete [] c_filename;
}

void
DefRec_DefSclFileS::unpack( char *& buffer, const VT_MPI_INT & bufferSize,
                            VT_MPI_INT & bufferPos )
{
   DefRec_BaseS::unpack( buffer, bufferSize, bufferPos );

   // filename.length()
   //
   uint32_t filename_length;
   CALL_MPI( MPI_Unpack( buffer, bufferSize, &bufferPos, &filename_length, 1,
                         MPI_UNSIGNED, MPI_COMM_WORLD ) );

   // filename
   //
   char * c_filename = new char[filename_length+1];
   vt_assert( c_filename );
   CALL_MPI( MPI_Unpack( buffer, bufferSize, &bufferPos, c_filename,
                         filename_length + 1, MPI_CHAR, MPI_COMM_WORLD ) );
   filename = c_filename;
   delete [] c_filename;
}

//////////////////// struct DefRec_DefSclS ////////////////////

VT_MPI_INT
DefRec_DefSclS::getPackSize()
{
   VT_MPI_INT buffer_size = DefRec_BaseS::getPackSize();
   VT_MPI_INT size;

   // sclfile + sclline
   //
   CALL_MPI( MPI_Pack_size( 2, MPI_UNSIGNED, MPI_COMM_WORLD, &size ) );
   buffer_size += size;

   return buffer_size;
}

void
DefRec_DefSclS::pack( char *& buffer, const VT_MPI_INT & bufferSize,
                      VT_MPI_INT & bufferPos )
{
   DefRec_BaseS::pack( buffer, bufferSize, bufferPos );

   // sclfile
   CALL_MPI( MPI_Pack( &sclfile, 1, MPI_UNSIGNED, buffer, bufferSize,
                       &bufferPos, MPI_COMM_WORLD ) );

   // sclline
   CALL_MPI( MPI_Pack( &sclline, 1, MPI_UNSIGNED, buffer, bufferSize,
                       &bufferPos, MPI_COMM_WORLD ) );
}

void
DefRec_DefSclS::unpack( char *& buffer, const VT_MPI_INT & bufferSize,
                        VT_MPI_INT & bufferPos )
{
   DefRec_BaseS::unpack( buffer, bufferSize, bufferPos );

   // sclfile
   CALL_MPI( MPI_Unpack( buffer, bufferSize, &bufferPos, &sclfile, 1,
                         MPI_UNSIGNED, MPI_COMM_WORLD ) );

   // sclline
   CALL_MPI( MPI_Unpack( buffer, bufferSize, &bufferPos, &sclline, 1,
                         MPI_UNSIGNED, MPI_COMM_WORLD ) );
}

//////////////////// struct DefRec_DefFileGroupS ////////////////////

VT_MPI_INT
DefRec_DefFileGroupS::getPackSize()
{
   VT_MPI_INT buffer_size = DefRec_BaseS::getPackSize();
   VT_MPI_INT size;

   // name.length()
   //
   CALL_MPI( MPI_Pack_size( 1, MPI_UNSIGNED, MPI_COMM_WORLD, &size ) );
   buffer_size += size;

   // name
   //
   CALL_MPI( MPI_Pack_size( name.length() + 1, MPI_CHAR, MPI_COMM_WORLD,
                            &size ) );
   buffer_size += size;

   return buffer_size;
}

void
DefRec_DefFileGroupS::pack( char *& buffer, const VT_MPI_INT & bufferSize,
                            VT_MPI_INT & bufferPos )
{
   DefRec_BaseS::pack( buffer, bufferSize, bufferPos );

   // name.length()
   //
   uint32_t name_length = name.length();
   CALL_MPI( MPI_Pack( &name_length, 1, MPI_UNSIGNED, buffer, bufferSize,
                       &bufferPos, MPI_COMM_WORLD ) );

   // name
   //
   char * c_name = new char[name_length+1];
   vt_assert( c_name );
   strcpy( c_name, name.c_str() );
   CALL_MPI( MPI_Pack( c_name, name_length + 1, MPI_CHAR, buffer, bufferSize,
                       &bufferPos, MPI_COMM_WORLD ) );
   delete [] c_name;
}

void
DefRec_DefFileGroupS::unpack( char *& buffer, const VT_MPI_INT & bufferSize,
                              VT_MPI_INT & bufferPos )
{
   DefRec_BaseS::unpack( buffer, bufferSize, bufferPos );

   // name.length()
   //
   uint32_t name_length;
   CALL_MPI( MPI_Unpack( buffer, bufferSize, &bufferPos, &name_length, 1,
                         MPI_UNSIGNED, MPI_COMM_WORLD ) );

   // name
   //
   char * c_name = new char[name_length+1];
   vt_assert( c_name );
   CALL_MPI( MPI_Unpack( buffer, bufferSize, &bufferPos, c_name,
                         name_length + 1, MPI_CHAR, MPI_COMM_WORLD ) );
   name = c_name;
   delete [] c_name;
}

//////////////////// struct DefRec_DefFileS ////////////////////

VT_MPI_INT
DefRec_DefFileS::getPackSize()
{
   VT_MPI_INT buffer_size = DefRec_BaseS::getPackSize();
   VT_MPI_INT size;

   // name.length() + group
   //
   CALL_MPI( MPI_Pack_size( 2, MPI_UNSIGNED, MPI_COMM_WORLD, &size ) );
   buffer_size += size;

   // name
   //
   CALL_MPI( MPI_Pack_size( name.length() + 1, MPI_CHAR, MPI_COMM_WORLD,
                            &size ) );
   buffer_size += size;

   return buffer_size;
}

void
DefRec_DefFileS::pack( char *& buffer, const VT_MPI_INT & bufferSize,
                       VT_MPI_INT & bufferPos )
{
   DefRec_BaseS::pack( buffer, bufferSize, bufferPos );

   // name.length()
   //
   uint32_t name_length = name.length();
   CALL_MPI( MPI_Pack( &name_length, 1, MPI_UNSIGNED, buffer, bufferSize,
                       &bufferPos, MPI_COMM_WORLD ) );

   // name
   //
   char * c_name = new char[name_length+1];
   vt_assert( c_name );
   strcpy( c_name, name.c_str() );
   CALL_MPI( MPI_Pack( c_name, name_length + 1, MPI_CHAR, buffer, bufferSize,
                       &bufferPos, MPI_COMM_WORLD ) );
   delete [] c_name;

   // group
   CALL_MPI( MPI_Pack( &group, 1, MPI_UNSIGNED, buffer, bufferSize, &bufferPos,
                       MPI_COMM_WORLD ) );
}

void
DefRec_DefFileS::unpack( char *& buffer, const VT_MPI_INT & bufferSize,
                         VT_MPI_INT & bufferPos )
{
   DefRec_BaseS::unpack( buffer, bufferSize, bufferPos );

   // name.length()
   //
   uint32_t name_length;
   CALL_MPI( MPI_Unpack( buffer, bufferSize, &bufferPos, &name_length, 1,
                         MPI_UNSIGNED, MPI_COMM_WORLD ) );

   // name
   //
   char * c_name = new char[name_length+1];
   vt_assert( c_name );
   CALL_MPI( MPI_Unpack( buffer, bufferSize, &bufferPos, c_name,
                         name_length + 1, MPI_CHAR, MPI_COMM_WORLD ) );
   name = c_name;
   delete [] c_name;

   // group
   CALL_MPI( MPI_Unpack( buffer, bufferSize, &bufferPos, &group, 1,
                         MPI_UNSIGNED, MPI_COMM_WORLD ) );
}

//////////////////// struct DefRec_DefFunctionGroupS ////////////////////

VT_MPI_INT
DefRec_DefFunctionGroupS::getPackSize()
{
   VT_MPI_INT buffer_size = DefRec_BaseS::getPackSize();
   VT_MPI_INT size;

   // name.length()
   //
   CALL_MPI( MPI_Pack_size( 1, MPI_UNSIGNED, MPI_COMM_WORLD, &size ) );
   buffer_size += size;

   // name
   //
   CALL_MPI( MPI_Pack_size( name.length() + 1, MPI_CHAR, MPI_COMM_WORLD,
                            &size ) );
   buffer_size += size;

   return buffer_size;
}

void
DefRec_DefFunctionGroupS::pack( char *& buffer, const VT_MPI_INT & bufferSize,
                                VT_MPI_INT & bufferPos )
{
   DefRec_BaseS::pack( buffer, bufferSize, bufferPos );

   // name.length()
   //
   uint32_t name_length = name.length();
   CALL_MPI( MPI_Pack( &name_length, 1, MPI_UNSIGNED, buffer, bufferSize,
                       &bufferPos, MPI_COMM_WORLD ) );

   // name
   //
   char * c_name = new char[name_length+1];
   vt_assert( c_name );
   strcpy( c_name, name.c_str() );
   CALL_MPI( MPI_Pack( c_name, name_length + 1, MPI_CHAR, buffer, bufferSize,
                       &bufferPos, MPI_COMM_WORLD ) );
   delete [] c_name;
}

void
DefRec_DefFunctionGroupS::unpack( char *& buffer, const VT_MPI_INT & bufferSize,
                                  VT_MPI_INT & bufferPos )
{
   DefRec_BaseS::unpack( buffer, bufferSize, bufferPos );

   // name.length()
   //
   uint32_t name_length;
   CALL_MPI( MPI_Unpack( buffer, bufferSize, &bufferPos, &name_length, 1,
                         MPI_UNSIGNED, MPI_COMM_WORLD ) );

   // name
   //
   char * c_name = new char[name_length+1];
   vt_assert( c_name );
   CALL_MPI( MPI_Unpack( buffer, bufferSize, &bufferPos, c_name,
                         name_length + 1, MPI_CHAR, MPI_COMM_WORLD ) );
   name = c_name;
   delete [] c_name;
}

//////////////////// struct DefRec_DefFunctionS ////////////////////

VT_MPI_INT
DefRec_DefFunctionS::getPackSize()
{
   VT_MPI_INT buffer_size = DefRec_BaseS::getPackSize();
   VT_MPI_INT size;

   // name.length() + group + scltoken
   //
   CALL_MPI( MPI_Pack_size( 3, MPI_UNSIGNED, MPI_COMM_WORLD, &size ) );
   buffer_size += size;

   // name
   //
   CALL_MPI( MPI_Pack_size( name.length() + 1, MPI_CHAR, MPI_COMM_WORLD,
                            &size ) );
   buffer_size += size;

   return buffer_size;
}

void
DefRec_DefFunctionS::pack( char *& buffer, const VT_MPI_INT & bufferSize,
                           VT_MPI_INT & bufferPos )
{
   DefRec_BaseS::pack( buffer, bufferSize, bufferPos );

   // name.length()
   //
   uint32_t name_length = name.length();
   CALL_MPI( MPI_Pack( &name_length, 1, MPI_UNSIGNED, buffer, bufferSize,
                       &bufferPos, MPI_COMM_WORLD ) );

   // name
   //
   char * c_name = new char[name_length+1];
   vt_assert( c_name );
   strcpy( c_name, name.c_str() );
   CALL_MPI( MPI_Pack( c_name, name_length + 1, MPI_CHAR, buffer, bufferSize,
                       &bufferPos, MPI_COMM_WORLD ) );
   delete [] c_name;

   // group
   CALL_MPI( MPI_Pack( &group, 1, MPI_UNSIGNED, buffer, bufferSize, &bufferPos,
                       MPI_COMM_WORLD ) );

   // scltoken
   CALL_MPI( MPI_Pack( &scltoken, 1, MPI_UNSIGNED, buffer, bufferSize,
                       &bufferPos, MPI_COMM_WORLD ) );
}

void
DefRec_DefFunctionS::unpack( char *& buffer, const VT_MPI_INT & bufferSize,
                             VT_MPI_INT & bufferPos )
{
   DefRec_BaseS::unpack( buffer, bufferSize, bufferPos );

   // name.length()
   //
   uint32_t name_length;
   CALL_MPI( MPI_Unpack( buffer, bufferSize, &bufferPos, &name_length, 1,
                         MPI_UNSIGNED, MPI_COMM_WORLD ) );

   // name
   //
   char * c_name = new char[name_length+1];
   vt_assert( c_name );
   CALL_MPI( MPI_Unpack( buffer, bufferSize, &bufferPos, c_name,
                         name_length + 1, MPI_CHAR, MPI_COMM_WORLD ) );
   name = c_name;
   delete [] c_name;

   // group
   CALL_MPI( MPI_Unpack( buffer, bufferSize, &bufferPos, &group, 1,
                         MPI_UNSIGNED, MPI_COMM_WORLD ) );

   // scltoken
   CALL_MPI( MPI_Unpack( buffer, bufferSize, &bufferPos, &scltoken, 1,
                         MPI_UNSIGNED, MPI_COMM_WORLD ) );
}

//////////////////// DefRec_DefCollOpS ////////////////////

VT_MPI_INT
DefRec_DefCollOpS::getPackSize()
{
   VT_MPI_INT buffer_size = DefRec_BaseS::getPackSize();
   VT_MPI_INT size;

   // name.length() + type
   //
   CALL_MPI( MPI_Pack_size( 2, MPI_UNSIGNED, MPI_COMM_WORLD, &size ) );
   buffer_size += size;

   // name
   //
   CALL_MPI( MPI_Pack_size( name.length() + 1, MPI_CHAR, MPI_COMM_WORLD,
                            &size ) );
   buffer_size += size;

   return buffer_size;
}

void
DefRec_DefCollOpS::pack( char *& buffer, const VT_MPI_INT & bufferSize,
                         VT_MPI_INT & bufferPos )
{
   DefRec_BaseS::pack( buffer, bufferSize, bufferPos );

   // name.length()
   //
   uint32_t name_length = name.length();
   CALL_MPI( MPI_Pack( &name_length, 1, MPI_UNSIGNED, buffer, bufferSize,
                       &bufferPos, MPI_COMM_WORLD ) );

   // name
   //
   char * c_name = new char[name_length+1];
   vt_assert( c_name );
   strcpy( c_name, name.c_str() );
   CALL_MPI( MPI_Pack( c_name, name_length + 1, MPI_CHAR, buffer, bufferSize,
                       &bufferPos, MPI_COMM_WORLD ) );
   delete [] c_name;

   // type
   CALL_MPI( MPI_Pack( &type, 1, MPI_UNSIGNED, buffer, bufferSize, &bufferPos,
                       MPI_COMM_WORLD ) );
}

void
DefRec_DefCollOpS::unpack( char *& buffer, const VT_MPI_INT & bufferSize,
                           VT_MPI_INT & bufferPos )
{
   DefRec_BaseS::unpack( buffer, bufferSize, bufferPos );

   // name.length()
   //
   uint32_t name_length;
   CALL_MPI( MPI_Unpack( buffer, bufferSize, &bufferPos, &name_length, 1,
                         MPI_UNSIGNED, MPI_COMM_WORLD ) );

   // name
   //
   char * c_name = new char[name_length+1];
   vt_assert( c_name );
   CALL_MPI( MPI_Unpack( buffer, bufferSize, &bufferPos, c_name,
                         name_length + 1, MPI_CHAR, MPI_COMM_WORLD ) );
   name = c_name;
   delete [] c_name;

   // type
   CALL_MPI( MPI_Unpack( buffer, bufferSize, &bufferPos, &type, 1, MPI_UNSIGNED,
                         MPI_COMM_WORLD ) );
}

//////////////////// DefRec_DefCounterGroupS ////////////////////

VT_MPI_INT
DefRec_DefCounterGroupS::getPackSize()
{
   VT_MPI_INT buffer_size = DefRec_BaseS::getPackSize();
   VT_MPI_INT size;

   // name.length()
   //
   CALL_MPI( MPI_Pack_size( 1, MPI_UNSIGNED, MPI_COMM_WORLD, &size ) );
   buffer_size += size;

   // name
   //
   CALL_MPI( MPI_Pack_size( name.length() + 1, MPI_CHAR, MPI_COMM_WORLD,
                            &size ) );
   buffer_size += size;

   return buffer_size;
}

void
DefRec_DefCounterGroupS::pack( char *& buffer, const VT_MPI_INT & bufferSize,
                               VT_MPI_INT & bufferPos )
{
   DefRec_BaseS::pack( buffer, bufferSize, bufferPos );

   // name.length()
   //
   uint32_t name_length = name.length();
   CALL_MPI( MPI_Pack( &name_length, 1, MPI_UNSIGNED, buffer, bufferSize,
                       &bufferPos, MPI_COMM_WORLD ) );

   // name
   //
   char * c_name = new char[name_length+1];
   vt_assert( c_name );
   strcpy( c_name, name.c_str() );
   CALL_MPI( MPI_Pack( c_name, name_length + 1, MPI_CHAR, buffer, bufferSize,
                       &bufferPos, MPI_COMM_WORLD ) );
   delete [] c_name;
}

void
DefRec_DefCounterGroupS::unpack( char *& buffer, const VT_MPI_INT & bufferSize,
                                 VT_MPI_INT & bufferPos )
{
   DefRec_BaseS::unpack( buffer, bufferSize, bufferPos );

   // name.length()
   //
   uint32_t name_length;
   CALL_MPI( MPI_Unpack( buffer, bufferSize, &bufferPos, &name_length, 1,
                         MPI_UNSIGNED, MPI_COMM_WORLD ) );

   // name
   //
   char * c_name = new char[name_length+1];
   vt_assert( c_name );
   CALL_MPI( MPI_Unpack( buffer, bufferSize, &bufferPos, c_name,
                         name_length + 1, MPI_CHAR, MPI_COMM_WORLD ) );
   name = c_name;
   delete [] c_name;
}

//////////////////// DefRec_DefCounterS ////////////////////

VT_MPI_INT
DefRec_DefCounterS::getPackSize()
{
   VT_MPI_INT buffer_size = DefRec_BaseS::getPackSize();
   VT_MPI_INT size;

   // name.length() + properties + group + unit.length()
   //
   CALL_MPI( MPI_Pack_size( 4, MPI_UNSIGNED, MPI_COMM_WORLD, &size ) );
   buffer_size += size;

   // name + unit
   //
   CALL_MPI( MPI_Pack_size( name.length() + 1 + unit.length() + 1, MPI_CHAR,
                            MPI_COMM_WORLD, &size ) );
   buffer_size += size;

   return buffer_size;
}

void
DefRec_DefCounterS::pack( char *& buffer, const VT_MPI_INT & bufferSize,
                          VT_MPI_INT & bufferPos )
{
   DefRec_BaseS::pack( buffer, bufferSize, bufferPos );

   // name.length()
   //
   uint32_t name_length = name.length();
   CALL_MPI( MPI_Pack( &name_length, 1, MPI_UNSIGNED, buffer, bufferSize,
                       &bufferPos, MPI_COMM_WORLD ) );

   // name
   //
   char * c_name = new char[name_length+1];
   vt_assert( c_name );
   strcpy( c_name, name.c_str() );
   CALL_MPI( MPI_Pack( c_name, name_length + 1, MPI_CHAR, buffer, bufferSize,
                       &bufferPos, MPI_COMM_WORLD ) );
   delete [] c_name;

   // properties
   CALL_MPI( MPI_Pack( &properties, 1, MPI_UNSIGNED, buffer, bufferSize,
                       &bufferPos, MPI_COMM_WORLD ) );

   // group
   CALL_MPI( MPI_Pack( &group, 1, MPI_UNSIGNED, buffer, bufferSize,
                       &bufferPos, MPI_COMM_WORLD ) );

   // unit.length()
   //
   uint32_t unit_length = unit.length();
   CALL_MPI( MPI_Pack( &unit_length, 1, MPI_UNSIGNED, buffer, bufferSize,
                       &bufferPos, MPI_COMM_WORLD ) );

   // unit
   //
   char * c_unit = new char[unit_length+1];
   vt_assert( c_unit );
   strcpy( c_unit, unit.c_str() );
   CALL_MPI( MPI_Pack( c_unit, unit_length + 1, MPI_CHAR, buffer, bufferSize,
                       &bufferPos, MPI_COMM_WORLD ) );
   delete [] c_unit;
}

void
DefRec_DefCounterS::unpack( char *& buffer, const VT_MPI_INT & bufferSize,
                            VT_MPI_INT & bufferPos )
{
   DefRec_BaseS::unpack( buffer, bufferSize, bufferPos );

   // name.length()
   //
   uint32_t name_length;
   CALL_MPI( MPI_Unpack( buffer, bufferSize, &bufferPos, &name_length, 1,
                         MPI_UNSIGNED, MPI_COMM_WORLD ) );

   // name
   //
   char * c_name = new char[name_length+1];
   vt_assert( c_name );
   CALL_MPI( MPI_Unpack( buffer, bufferSize, &bufferPos, c_name,
                         name_length + 1, MPI_CHAR, MPI_COMM_WORLD ) );
   name = c_name;
   delete [] c_name;

   // properties
   CALL_MPI( MPI_Unpack( buffer, bufferSize, &bufferPos, &properties, 1,
                         MPI_UNSIGNED, MPI_COMM_WORLD ) );

   // group
   CALL_MPI( MPI_Unpack( buffer, bufferSize, &bufferPos, &group, 1,
                         MPI_UNSIGNED, MPI_COMM_WORLD ) );

   // unit.length()
   uint32_t unit_length;
   CALL_MPI( MPI_Unpack( buffer, bufferSize, &bufferPos, &unit_length, 1,
                         MPI_UNSIGNED, MPI_COMM_WORLD ) );

   // unit
   //
   char * c_unit = new char[unit_length+1];
   vt_assert( c_unit );
   CALL_MPI( MPI_Unpack( buffer, bufferSize, &bufferPos, c_unit,
                         unit_length + 1, MPI_CHAR, MPI_COMM_WORLD ) );
   unit = c_unit;
   delete [] c_unit;
}

//////////////////// DefRec_DefCounterAssignmentsS ////////////////////

VT_MPI_INT
DefRec_DefCounterAssignmentsS::getPackSize()
{
   VT_MPI_INT buffer_size = DefRec_BaseS::getPackSize();
   VT_MPI_INT size;

   // groups.size() + groups
   //
   CALL_MPI( MPI_Pack_size( 1 + groups.size(), MPI_UNSIGNED, MPI_COMM_WORLD,
                            &size ) );
   buffer_size += size;

   return buffer_size;
}

void
DefRec_DefCounterAssignmentsS::pack( char *& buffer,
                                     const VT_MPI_INT & bufferSize,
                                     VT_MPI_INT & bufferPos )
{
   DefRec_BaseS::pack( buffer, bufferSize, bufferPos );

   // groups.size()
   //
   uint32_t groups_size = groups.size();
   CALL_MPI( MPI_Pack( &groups_size, 1, MPI_UNSIGNED, buffer, bufferSize,
                       &bufferPos, MPI_COMM_WORLD ) );

   // groups
   //
   for( std::set<uint32_t>::const_iterator it = groups.begin();
        it != groups.end(); ++it )
   {
      uint32_t group = *it;
      CALL_MPI( MPI_Pack( &group, 1, MPI_UNSIGNED, buffer, bufferSize,
                          &bufferPos, MPI_COMM_WORLD ) );
   }
}

void
DefRec_DefCounterAssignmentsS::unpack( char *& buffer,
                                       const VT_MPI_INT & bufferSize,
                                       VT_MPI_INT & bufferPos )
{
   DefRec_BaseS::unpack( buffer, bufferSize, bufferPos );

   // groups.size()
   //
   uint32_t groups_size;
   CALL_MPI( MPI_Unpack( buffer, bufferSize, &bufferPos, &groups_size, 1,
                         MPI_UNSIGNED, MPI_COMM_WORLD ) );

   // groups
   //
   for( uint32_t i = 0; i < groups_size; i++ )
   {
      uint32_t group;
      CALL_MPI( MPI_Unpack( buffer, bufferSize, &bufferPos, &group, 1,
                            MPI_UNSIGNED, MPI_COMM_WORLD ) );
      groups.insert( group );
   }
}

//////////////////// DefRec_DefKeyValueS ////////////////////

VT_MPI_INT
DefRec_DefKeyValueS::getPackSize()
{
   VT_MPI_INT buffer_size = DefRec_BaseS::getPackSize();
   VT_MPI_INT size;

   // type + name.length()
   //
   CALL_MPI( MPI_Pack_size( 2, MPI_UNSIGNED, MPI_COMM_WORLD, &size ) );
   buffer_size += size;

   // name
   //
   CALL_MPI( MPI_Pack_size( name.length() + 1, MPI_CHAR, MPI_COMM_WORLD,
                            &size ) );
   buffer_size += size;

   return buffer_size;
}

void
DefRec_DefKeyValueS::pack( char *& buffer, const VT_MPI_INT & bufferSize,
                           VT_MPI_INT & bufferPos )
{
   DefRec_BaseS::pack( buffer, bufferSize, bufferPos );

   // type
   CALL_MPI( MPI_Pack( &type, 1, MPI_UNSIGNED, buffer, bufferSize, &bufferPos,
                       MPI_COMM_WORLD ) );

   // name.length()
   //
   uint32_t name_length = name.length();
   CALL_MPI( MPI_Pack( &name_length, 1, MPI_UNSIGNED, buffer, bufferSize,
                       &bufferPos, MPI_COMM_WORLD ) );

   // name
   //
   char * c_name = new char[name_length+1];
   vt_assert( c_name );
   strcpy( c_name, name.c_str() );
   CALL_MPI( MPI_Pack( c_name, name_length + 1, MPI_CHAR, buffer, bufferSize,
                       &bufferPos, MPI_COMM_WORLD ) );
   delete [] c_name;
}

void
DefRec_DefKeyValueS::unpack( char *& buffer, const VT_MPI_INT & bufferSize,
                             VT_MPI_INT & bufferPos )
{
   DefRec_BaseS::unpack( buffer, bufferSize, bufferPos );

   // type
   CALL_MPI( MPI_Unpack( buffer, bufferSize, &bufferPos, &type, 1,
                         MPI_UNSIGNED, MPI_COMM_WORLD ) );

   // name.length()
   //
   uint32_t name_length;
   CALL_MPI( MPI_Unpack( buffer, bufferSize, &bufferPos, &name_length, 1,
                         MPI_UNSIGNED, MPI_COMM_WORLD ) );

   // name
   //
   char * c_name = new char[name_length+1];
   vt_assert( c_name );
   CALL_MPI( MPI_Unpack( buffer, bufferSize, &bufferPos, c_name,
                         name_length + 1, MPI_CHAR, MPI_COMM_WORLD ) );
   name = c_name;
   delete [] c_name;
}

//////////////////// DefRec_DefMarkerS ////////////////////

VT_MPI_INT
DefRec_DefMarkerS::getPackSize()
{
   VT_MPI_INT buffer_size = DefRec_BaseS::getPackSize();
   VT_MPI_INT size;

   // type + name.length()
   //
   CALL_MPI( MPI_Pack_size( 2, MPI_UNSIGNED, MPI_COMM_WORLD, &size ) );
   buffer_size += size;

   // name
   //
   CALL_MPI( MPI_Pack_size( name.length() + 1, MPI_CHAR, MPI_COMM_WORLD,
                            &size ) );
   buffer_size += size;

   return buffer_size;
}

void
DefRec_DefMarkerS::pack( char *& buffer, const VT_MPI_INT & bufferSize,
                         VT_MPI_INT & bufferPos )
{
   DefRec_BaseS::pack( buffer, bufferSize, bufferPos );

   // type
   CALL_MPI( MPI_Pack( &type, 1, MPI_UNSIGNED, buffer, bufferSize, &bufferPos,
                       MPI_COMM_WORLD ) );

   // name.length()
   //
   uint32_t name_length = name.length();
   CALL_MPI( MPI_Pack( &name_length, 1, MPI_UNSIGNED, buffer, bufferSize,
                       &bufferPos, MPI_COMM_WORLD ) );

   // name
   //
   char * c_name = new char[name_length+1];
   vt_assert( c_name );
   strcpy( c_name, name.c_str() );
   CALL_MPI( MPI_Pack( c_name, name_length + 1, MPI_CHAR, buffer, bufferSize,
                       &bufferPos, MPI_COMM_WORLD ) );
   delete [] c_name;
}

void
DefRec_DefMarkerS::unpack( char *& buffer, const VT_MPI_INT & bufferSize,
                           VT_MPI_INT & bufferPos )
{
   DefRec_BaseS::unpack( buffer, bufferSize, bufferPos );

   // type
   CALL_MPI( MPI_Unpack( buffer, bufferSize, &bufferPos, &type, 1, MPI_UNSIGNED,
                         MPI_COMM_WORLD ) );

   // name.length()
   //
   uint32_t name_length;
   CALL_MPI( MPI_Unpack( buffer, bufferSize, &bufferPos, &name_length, 1,
                         MPI_UNSIGNED, MPI_COMM_WORLD ) );

   // name
   //
   char * c_name = new char[name_length+1];
   vt_assert( c_name );
   CALL_MPI( MPI_Unpack( buffer, bufferSize, &bufferPos, c_name,
                         name_length + 1, MPI_CHAR, MPI_COMM_WORLD ) );
   name = c_name;
   delete [] c_name;
}

#endif // VT_MPI

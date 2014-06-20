/*
 This is part of the OTF library. Copyright by ZIH, TU Dresden 2005-2013.
 Authors: Andreas Knuepfer, Holger Brunst, Ronny Brendel, Thomas Kriebitzsch
*/

#ifndef PYOTF_AUXILIARYFUNCTIONS_H
#define PYOTF_AUXILIARYFUNCTIONS_H

uint32_t* createInt32ArrayFromSequence( PyObject* list );
uint8_t* createInt8ArrayFromSequence( PyObject* list );
PyObject* createSequenceFromInt8Array( uint8_t* array, uint8_t len );

/* conversion from python sequence to uint32_t array
CREATES AN ARRAY - you have to free it yourself */
uint32_t* createInt32ArrayFromSequence( PyObject* list ) {

	int i;
	int dim;
	uint32_t* ret;

	if (!PySequence_Check( list )) {
#ifdef OTF_VERBOSE
		PyErr_SetString(PyExc_TypeError,"Expecting a sequence");
#endif /* OTF_VERBOSE */
		return NULL;
	}
	
	dim= PyObject_Length( list );

	ret= (uint32_t*) malloc( sizeof(uint32_t) * dim );

	for ( i =0; i < dim; ++i ) {
	
		PyObject *o = PySequence_GetItem( list ,i );
		
		if (!PyInt_Check(o)) {
			Py_XDECREF(o);
#ifdef OTF_VERBOSE
			PyErr_SetString(PyExc_ValueError,"Expecting a sequence of integers");
#endif /* OTF_VERBOSE */
			free( ret );
			return NULL;
		}
		
		ret[i] = PyInt_AsLong(o);
		Py_DECREF(o);
	}
	
	return ret;
}

/* conversion from python sequence to uint8_t array
CREATES AN ARRAY - you have to free it yourself */
uint8_t* createInt8ArrayFromSequence( PyObject* list ) {

	int i;
	int dim;
	uint8_t* ret;

	if (!PySequence_Check( list )) {
#ifdef OTF_VERBOSE
		PyErr_SetString(PyExc_TypeError,"Expecting a sequence");
#endif /* OTF_VERBOSE */
		return NULL;
	}
	
	dim= PyObject_Length( list );

	ret= (uint8_t*) malloc( sizeof(uint8_t) * dim );

	for ( i =0; i < dim; ++i ) {
	
		PyObject *o = PySequence_GetItem( list ,i );
		
		if (!PyInt_Check(o)) {
			Py_XDECREF(o);
#ifdef OTF_VERBOSE
			PyErr_SetString(PyExc_ValueError,"Expecting a sequence of integers");
#endif /* OTF_VERBOSE */
			free( ret );
			return NULL;
		}
		
		ret[i] = PyInt_AsLong(o);
		Py_DECREF(o);
	}
	
	return ret;
}

/* conversion from uint8_t array to python list,
   creates a python list and returns it */
PyObject* createSequenceFromInt8Array( uint8_t* array, uint8_t len ) {
	
	uint8_t i;
	PyObject *pylist = PyList_New(len);

	if( ! PyList_Check(pylist) ) {
		printf("Creating PyList not possible\n");
	}

	for(i = 0; i < len; i++) {
		PyList_SetItem( pylist, i, PyInt_FromLong(array[i]) );
	}

	
	return pylist;

}


#endif /* PYOTF_AUXILIARYFUNCTIONS_H */

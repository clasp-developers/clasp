//
// (C) 2004 Christian E. Schafmeister
//
#define	TURN_DEBUG_ON

//
// coordSys.cc
//

#include	"foundation.h"
#include	"quickDom.h"
#include	"xmlGraphics.h"
#include	"coordSys.h"

#define	PI		3.1415926535897932
#define	DEGTORAD	0.0174533
#define	PARALLEL_CUTOFF	0.00001

#ifdef	USEBOOSTPYTHON
void	
InitPython_CoordinateSystem()
{


    py::class_<Dumb_XConventionEulerTransform,
	boost::shared_ptr<Dumb_XConventionEulerTransform>,
	boost::noncopyable> ("XConventionEulerTransform")
	.def("defineForCoordinateSystem", 
		&Dumb_XConventionEulerTransform::defineForCoordinateSystem )
	.def("defineForCoordinateSystemToCoordinateSystem", 
		&Dumb_XConventionEulerTransform::defineForCoordinateSystemToCoordinateSystem )
	.def("matrixFromCanonical", &Dumb_XConventionEulerTransform::matrixFromCanonical)
	.def("matrixToCanonical", &Dumb_XConventionEulerTransform::matrixToCanonical)
	.def("getCoordinateSystem", &Dumb_XConventionEulerTransform::getCoordinateSystem)
	.def("asXml", &Dumb_XConventionEulerTransform::asXml)
	.def("asString", &Dumb_XConventionEulerTransform::asString)
	.def("getAlpha", &Dumb_XConventionEulerTransform::getAlpha)
	.def("getBeta", &Dumb_XConventionEulerTransform::getBeta)
	.def("getGamma", &Dumb_XConventionEulerTransform::getGamma)
	.def("getPhi", &Dumb_XConventionEulerTransform::getPhi)
	.def("getTheta", &Dumb_XConventionEulerTransform::getTheta)
	.def("getDistance", &Dumb_XConventionEulerTransform::getDistance)
	.def("setAlpha", &Dumb_XConventionEulerTransform::setAlpha)
	.def("setBeta", &Dumb_XConventionEulerTransform::setBeta)
	.def("setGamma", &Dumb_XConventionEulerTransform::setGamma)
	.def("setPhi", &Dumb_XConventionEulerTransform::setPhi)
	.def("setTheta", &Dumb_XConventionEulerTransform::setTheta)
	.def("setDistance", &Dumb_XConventionEulerTransform::setDistance)
//	.def("parseFromXml", &Dumb_XConventionEulerTransform::parseFromXml)
    ;
    py::class_<Dumb_GimbalTransform,
	boost::shared_ptr<Dumb_GimbalTransform>,
	boost::noncopyable> ("GimbalTransform")
	.def("defineForCoordinateSystem", 
		&Dumb_GimbalTransform::defineForCoordinateSystem )
	.def("defineForCoordinateSystemToCoordinateSystem", 
		&Dumb_GimbalTransform::defineForCoordinateSystemToCoordinateSystem )
	.def("matrixFromCanonical", &Dumb_GimbalTransform::matrixFromCanonical)
	.def("matrixToCanonical", &Dumb_GimbalTransform::matrixToCanonical)
	.def("getCoordinateSystem", &Dumb_GimbalTransform::getCoordinateSystem)
	.def("asXml", &Dumb_GimbalTransform::asXml)
	.def("asString", &Dumb_GimbalTransform::asString)
	.def("asLine", &Dumb_GimbalTransform::asLine)
	.def("getRotX", &Dumb_GimbalTransform::getRotX)
	.def("getRotY", &Dumb_GimbalTransform::getRotY)
	.def("getRotZ", &Dumb_GimbalTransform::getRotZ)
	.def("getRotYY", &Dumb_GimbalTransform::getRotYY)
	.def("getRotZZ", &Dumb_GimbalTransform::getRotZZ)
	.def("getDistance", &Dumb_GimbalTransform::getDistance)
	.def("setRotX", &Dumb_GimbalTransform::setRotX)
	.def("setRotY", &Dumb_GimbalTransform::setRotY)
	.def("setRotZ", &Dumb_GimbalTransform::setRotZ)
	.def("setRotYY", &Dumb_GimbalTransform::setRotYY)
	.def("setRotZZ", &Dumb_GimbalTransform::setRotZZ)
	.def("setRotXDegrees", &Dumb_GimbalTransform::setRotXDegrees)
	.def("setRotYDegrees", &Dumb_GimbalTransform::setRotYDegrees)
	.def("setRotZDegrees", &Dumb_GimbalTransform::setRotZDegrees)
	.def("setRotYYDegrees", &Dumb_GimbalTransform::setRotYYDegrees)
	.def("setRotZZDegrees", &Dumb_GimbalTransform::setRotZZDegrees)
	.def("setDistance", &Dumb_GimbalTransform::setDistance)
	.def("getDirection", &Dumb_GimbalTransform::getDirection)
	.def("parseFromXml", &Dumb_GimbalTransform::parseFromXml)
    ;

    py::class_<Dumb_CoordinateSystem,
	boost::shared_ptr<Dumb_CoordinateSystem>,
	boost::noncopyable> ("CoordinateSystem")
	.def("getOrigin", &Dumb_CoordinateSystem::getOrigin)
	.def("setOrigin", &Dumb_CoordinateSystem::setOrigin)
	.def("getX", &Dumb_CoordinateSystem::getX)
	.def("getY", &Dumb_CoordinateSystem::getY)
	.def("getZ", &Dumb_CoordinateSystem::getZ)
	.def("canonical", &Dumb_CoordinateSystem::canonical)
	.def("defineForAtoms", &Dumb_CoordinateSystem::defineForAtoms)
	.def("defineForVectorsOriginXDirYDir", &Dumb_CoordinateSystem::defineForVectorsOriginXDirYDir)
	.def("defineForVectorsOriginXDirXYPlane", &Dumb_CoordinateSystem::defineForVectorsOriginXDirXYPlane)
	.def("defineForVectorsOriginZDirXZPlane", &Dumb_CoordinateSystem::defineForVectorsOriginZDirXZPlane)
	.def("defineForVectorsOriginXDirZDir", &Dumb_CoordinateSystem::defineForVectorsOriginXDirZDir)
	.def("defineRandom", &Dumb_CoordinateSystem::defineRandom)
	.def("sameAs", &Dumb_CoordinateSystem::sameAs)
	.def("matrixFromCanonical", &Dumb_CoordinateSystem::matrixFromCanonical)
	.def("matrixToCanonical", &Dumb_CoordinateSystem::matrixToCanonical)
	.def("renderXml", &Dumb_CoordinateSystem::renderXml)
	.def("dump", &Dumb_CoordinateSystem::dump)
	.def("copy", &Dumb_CoordinateSystem::copy)
	.def("asXml", &Dumb_CoordinateSystem::asXml)
	.def("transformWithMatrix", &Dumb_CoordinateSystem::transformWithMatrix)
	;


}
#endif




Dumb_GimbalTransform::Dumb_GimbalTransform()
{
    REF_CREATE("Dumb_GimbalTransform");
}

Dumb_GimbalTransform::~Dumb_GimbalTransform()
{
    REF_DELETE("Dumb_GimbalTransform");
}
void	Dumb_GimbalTransform::defineForCoordinateSystem(RPCoordinateSystem coord)
{
_FUNCTION_TRACE("Dumb_GimbalTransform::defineForCoordinateSystem");
double	sgn;
	Vector3	labX; labX.set(1.0,0.0,0.0);
	Vector3	labY; labY.set(0.0,1.0,0.0);
	Vector3	labZ; labZ.set(0.0,0.0,1.0);
        Vector3 c2xo = coord->getX();
        Vector3 xycomp = Vector3( c2xo.getX(), c2xo.getY(), 0.0 ).normalized();
        VP0(("xycomp={%lf,%lf,%lf};",xycomp.getX(),xycomp.getY(),xycomp.getZ()));
	double cosXy = xycomp.dotProduct(labX);
        VP0(("cosXy = %lf;",cosXy));
        if ( xycomp.dotProduct(labY)>0 ) {
	    this->_RotZ = acos(cosXy);
        } else {
	    this->_RotZ = -acos(cosXy);
	}
	VP0(( "this->_RotZ = %lf degrees", this->_RotZ/DEGTORAD ));
            // 
            // Now undo the effect of rotz on c2.getX()
	    // 
        Matrix mrotz;
        mrotz.rightHandedRotationZ(-this->_RotZ);
        Vector3 c2x; c2x = mrotz.multiplyByVector3(c2xo);
	double cosC2x = c2x.dotProduct(labX);
        if ( c2x.dotProduct(labZ) < 0.0 ) {
	    this->_RotY = acos(cosC2x);
        } else {
	    this->_RotY = -acos(cosC2x);
	}
	VP0(( "this->_RotY = %lf degrees", this->_RotY/DEGTORAD ));
        Matrix mroty;
        mroty.rightHandedRotationY(-this->_RotY);
        Vector3 c3x = mroty.multiplyByVector3(c2x);
        Vector3 c2y = mrotz.multiplyByVector3(coord->getY());
        Vector3 c3y = mroty.multiplyByVector3(c2y);
    //    renderRay(self._gf,YELLOW,c1.getOrigin(),c3y)
        if ( c3y.dotProduct(labZ) > 0.0 ) {
            this->_RotX = acos(c3y.dotProduct(labY));
        } else {
	    this->_RotX = -acos(c3y.dotProduct(labY));
	}
	VP0(( "this->_RotX = %lf degrees", this->_RotX/DEGTORAD ));
        Vector3 trans = coord->getOrigin();
        Vector3 transNorm = trans.normalized();
        VP0(("trans= {%lf,%lf,%lf};", 
		trans.getX(),trans.getY(),trans.getZ() ));
        xycomp = Vector3( trans.getX(), trans.getY(),0.0 ).normalized();
        VP0(("xycomp= {%lf,%lf,%lf};", 
		xycomp.getX(),xycomp.getY(),xycomp.getZ() ));
	cosXy = xycomp.dotProduct(labX);
	VP0(( "cosXy = %lf", cosXy));
        if ( xycomp.dotProduct(labY)>0 ) {
	    this->_RotZZ = acos(cosXy);
        } else {
	    this->_RotZZ = -acos(cosXy);
	}
	VP0(( "this->_RotZZ = %lf degrees", this->_RotZZ/DEGTORAD ));
            // 
            // Now undo the effect of rotz on c2.getX()
	    // 
        mrotz.rightHandedRotationZ(-this->_RotZZ);
        c2x = mrotz.multiplyByVector3(transNorm);
        VP0(("c2x= {%lf,%lf,%lf};", c2x.getX(),c2x.getY(),c2x.getZ() ));
	cosC2x = c2x.dotProduct(labX);
	VP0(( "cosC2x = %lf", cosC2x ));
        if ( c2x.dotProduct(labZ) < 0.0 ) {
	    this->_RotYY = acos(cosC2x);
        } else {
	    this->_RotYY = -acos(cosC2x);
	}
	VP0(( "this->_RotYY = %lf degrees", this->_RotYY/DEGTORAD ));
        this->_Distance = trans.length();
	VP0(( "this->_Distance = %lf", this->_Distance ));
#if 0 //[
//Vector3		translate;
//Vector3		labZ, labX, node, nodeNorm;
//double		cosRotY, gamma, beta, alpha, distance, theta, phi;
//	// Calculate RotZ, the angle around the Z-axis I need to rotate X into the lab XY-plane
//
//    labZ.set(0.0,0.0,1.0);
//    labX.set(1.0,0.0,0.0);
//    node = labZ.crossProduct(coord->getZ());
//    VP0(("labZ {%lf,%lf,%lf}", labZ.getX(),labZ.getY(),labZ.getZ() ));
//    VP0(("labX {%lf,%lf,%lf}", labX.getX(),labX.getY(),labX.getZ() ));
//    VP0(("coordX = {%lf,%lf,%lf};",
//		coord->getX().getX(),coord->getX().getY(),coord->getX().getZ() ));
//    VP0(("coordY = {%lf,%lf,%lf};",
//		coord->getY().getX(), coord->getY().getY(), coord->getY().getZ() ));
//    VP0(("coordZ = {%lf,%lf,%lf};",
//		coord->getZ().getX(), coord->getZ().getY(), coord->getZ().getZ() ));
//    cosRotY = labZ.dotProduct(coord->getZ());
//    VP0(("cosRotY = %lf", cosRotY ));
//    VP0(("node.length= %lf", node.length()));
//    if ( fabs(node.length())<PARALLEL_CUTOFF ) {
//	VP0(( "node.length is zero"));
//	gamma = 0.0; // If the labZ and coord->getZ() are parallel or anti-parallel then gamma=
//        VP0(( "gamma = %lf deg", gamma/DEGTORAD ));
//	beta = (cosRotY>=0.0)?0.0:(180.0*DEGTORAD);
//        VP0(( "beta = %lf deg", beta/DEGTORAD ));
//        VP0(( "Calculating alpha"));
//        alpha = labX.angleToVectorAboutNormal(coord->getX(),labZ);
//        VP0(("alpha= %lf deg", alpha/DEGTORAD ));
//    } else {
//	VP0(( "node.length is not zero"));
//		// RotY range is [0,PI], arc
//	nodeNorm = node.normalized();
//        VP0(("nodeNorm =     {%lf,%lf,%lf};",nodeNorm.getX(), nodeNorm.getY(), nodeNorm.getZ() ));
//	VP0(( "About to calculate gamma"));
//	gamma = nodeNorm.angleToVectorAboutNormal(coord->getX(),coord->getZ());
//        VP0(( "gamma = %lf deg", gamma/DEGTORAD ));
//	beta = acos(cosRotY);
//        VP0(( "beta = %lf deg", beta/DEGTORAD ));
//        VP0(( "Calculating alpha"));
//        alpha = labX.angleToVectorAboutNormal(nodeNorm,labZ);
//        VP0(("alpha= %lf deg", alpha/DEGTORAD ));
//    }
//
//	// Now calculate RotYY and RotZZ
//
//    translate = coord->getOrigin();
//    VP0(("translate {%lf,%lf,%lf}", translate.getX(),translate.getY(),translate.getZ() ));
//    distance = translate.length();
//    VP0(( "distance = %lf", distance ));
//    if ( distance < 0.00000001 ) {
//        VP0(( "distance is zero" ));
//	phi = 0.0;
//	theta = 0.0;
//    } else {
//        VP0(( "distance is not zero" ));
//	Vector3 tn = translate.normalized();
//	double cosRotZZ = tn.dotProduct(labZ);
//	VP0(("cosRotZZ = %lf", cosRotZZ ));
//	Vector3 tnorm= labZ.crossProduct(tn);
//	if ( fabs(tnorm.length()) < PARALLEL_CUTOFF ) {
//	    VP0(( "tnorm.length is zero "));
//	    theta = (cosRotZZ>=0.0)?0.0:(180.0*DEGTORAD);
//	    phi = 0.0;
//	} else {
//	    VP0(( "tnorm.length is not zero "));
//	    theta = acos(cosRotZZ);
//	    phi = labX.angleToVectorAboutNormal(tnorm,labZ);
//	}
//	VP0(( "theta = %lf deg", theta/DEGTORAD ));
//	VP0(( "phi = %lf deg", phi/DEGTORAD ));
//    }
//    VP0(( "Setting values"));
//    this->_RotX = alpha;
//    this->_RotY = beta;
//    this->_RotZ = gamma;
//    this->_Distance = distance;
//    this->_RotZZ = theta;
//    this->_RotYY = phi;
#endif //]
}

string	Dumb_GimbalTransform::asString(const string& prefix)
{
stringstream	ss;
    ss << prefix << "RotX  = " << this->_RotX/0.0174533 << " deg" << endl;
    ss << prefix << "RotY  = " << this->_RotY/0.0174533 << " deg" << endl;
    ss << prefix << "RotZ  = " << this->_RotZ/0.0174533 << " deg" << endl;
    ss << prefix << "RotYY = " << this->_RotYY/0.0174533 << " deg" << endl;
    ss << prefix << "RotZZ = " << this->_RotZZ/0.0174533 << " deg" << endl;
    ss << prefix << "Dist  = " << this->_Distance << " A" << endl;
    return ss.str();
}

string	Dumb_GimbalTransform::asLine(bool data)
{
stringstream	ss;
    if ( !data ) {
	ss << "# RotX(deg) RotY(deg) RotZ(deg) RotYY(deg) RotZZ(deg) Distance(A)"; 
    } else {
	ss << this->_RotX/0.0174533 << " ";
	ss << this->_RotY/0.0174533 << " " ;
	ss << this->_RotZ/0.0174533 << " " ;
	ss << this->_RotYY/0.0174533 << " ";
	ss << this->_RotZZ/0.0174533 << " ";
	ss << this->_Distance;
    }
    return ss.str();
}

void	Dumb_GimbalTransform::defineForCoordinateSystemToCoordinateSystem(RPCoordinateSystem start, RPCoordinateSystem dest)
{
Matrix	toCanonical;
RPCoordinateSystem	transformedDest;

    toCanonical = start->matrixToCanonical();
    transformedDest = dest->copy();
    transformedDest->transformWithMatrix(toCanonical);
    this->defineForCoordinateSystem(transformedDest);
}


RPQDomNode	Dumb_GimbalTransform::asXml()
{
RPQDomNode	n;
    n = new_RPQDomNode("EulerTransform");
    n->addAttributeDouble("RotX",this->_RotX, 8, 5 );
    n->addAttributeDouble("RotY",this->_RotY, 8, 5 );
    n->addAttributeDouble("RotZ",this->_RotZ, 8, 5 );
    n->addAttributeDouble("Dist",this->_Distance, 8, 5 );
    n->addAttributeDouble("RotYY",this->_RotYY, 8, 5 );
    n->addAttributeDouble("RotZZ",this->_RotZZ, 8, 5 );
    return n;
}

void	Dumb_GimbalTransform::parseFromXml(RPQDomNode& node )
{
    this->_RotX = node->getAttributeDouble("RotX");
    this->_RotY = node->getAttributeDouble("RotY");
    this->_RotZ = node->getAttributeDouble("RotZ");
    this->_RotYY = node->getAttributeDouble("RotYY");
    this->_RotZZ = node->getAttributeDouble("RotZZ");
    this->_Distance = node->getAttributeDouble("Dist");
}

RPCoordinateSystem	Dumb_GimbalTransform::getCoordinateSystem()
{
Matrix		mt;
RPCoordinateSystem	c;

    c = new_RPCoordinateSystem();
    mt = this->matrixFromCanonical();
    c->transformWithMatrix(mt);
    return c;
}


Vector3	Dumb_GimbalTransform::getDirection()
{
    Vector3 vTrans; vTrans.set(1.0,0.0,0.0);
    Matrix mRotYY; mRotYY.rightHandedRotationY(this->_RotYY);
    Matrix mRotZZ; mRotZZ.rightHandedRotationZ(this->_RotZZ);
    vTrans = mRotYY.multiplyByVector3(vTrans);
    vTrans = mRotZZ.multiplyByVector3(vTrans);
    return vTrans;
}




Matrix	Dumb_GimbalTransform::matrixFromCanonical()
{
_FUNCTION_TRACE("Dumb_GimbalTransform::matrixFromCanonical");
    Matrix mX; mX.rightHandedRotationX(this->_RotX);
    Matrix mY; mY.rightHandedRotationY(this->_RotY);
    Matrix mZ; mZ.rightHandedRotationZ(this->_RotZ);
    Vector3 vTrans = this->getDirection();
    vTrans = vTrans.multiplyByScalar(this->_Distance);
    Matrix mTranslate; mTranslate.translate(&vTrans);
    VP0(("mTranslate = \n%s", mTranslate.asXml()->asString().c_str() ));
    Matrix mA = mY*mX;
    mA = mZ*mA;
    VP0(("mA = \n%s", mA.asXml()->asString().c_str() ));
    mA = mTranslate*mA;
    VP0(("mTransform = \n%s", mA.asXml()->asString().c_str() ));
    return mA;
}


Matrix	Dumb_GimbalTransform::matrixToCanonical()
{
RPCoordinateSystem	c;
    c = this->getCoordinateSystem();
    return c->matrixToCanonical();
}




Dumb_XConventionEulerTransform::Dumb_XConventionEulerTransform()
{
    REF_CREATE("Dumb_XConventionEulerTransform");
}

Dumb_XConventionEulerTransform::~Dumb_XConventionEulerTransform()
{
    REF_DELETE("Dumb_XConventionEulerTransform");
}

void	Dumb_XConventionEulerTransform::defineForCoordinateSystem(RPCoordinateSystem coord)
{
_FUNCTION_TRACE("Dumb_XConventionEulerTransform::defineForCoordinateSystem");
Vector3		translate;
Vector3		labZ, labX, node, nodeNorm;
double		cosBeta, gamma, beta, alpha, distance, theta, phi;
	// Calculate Gamma, the angle around the Z-axis I need to rotate X into the lab XY-plane

    labZ.set(0.0,0.0,1.0);
    labX.set(1.0,0.0,0.0);
    node = labZ.crossProduct(coord->getZ());
    VP0(("labZ {%lf,%lf,%lf}", labZ.getX(),labZ.getY(),labZ.getZ() ));
    VP0(("labX {%lf,%lf,%lf}", labX.getX(),labX.getY(),labX.getZ() ));
    VP0(("coordX = {%lf,%lf,%lf};",
		coord->getX().getX(),coord->getX().getY(),coord->getX().getZ() ));
    VP0(("coordY = {%lf,%lf,%lf};",
		coord->getY().getX(), coord->getY().getY(), coord->getY().getZ() ));
    VP0(("coordZ = {%lf,%lf,%lf};",
		coord->getZ().getX(), coord->getZ().getY(), coord->getZ().getZ() ));
    cosBeta = labZ.dotProduct(coord->getZ());
    VP0(("cosBeta = %lf", cosBeta ));
    VP0(("node.length= %lf", node.length()));
    if ( fabs(node.length())<PARALLEL_CUTOFF ) {
	VP0(( "node.length is zero"));
	gamma = 0.0; // If the labZ and coord->getZ() are parallel or anti-parallel then gamma=
        VP0(( "gamma = %lf deg", gamma/DEGTORAD ));
	beta = (cosBeta>=0.0)?0.0:(180.0*DEGTORAD);
        VP0(( "beta = %lf deg", beta/DEGTORAD ));
        VP0(( "Calculating alpha"));
        alpha = labX.angleToVectorAboutNormal(coord->getX(),labZ);
        VP0(("alpha= %lf deg", alpha/DEGTORAD ));
    } else {
	VP0(( "node.length is not zero"));
		// Beta range is [0,PI], arc
	nodeNorm = node.normalized();
        VP0(("nodeNorm =     {%lf,%lf,%lf};",nodeNorm.getX(), nodeNorm.getY(), nodeNorm.getZ() ));
	VP0(( "About to calculate gamma"));
	gamma = nodeNorm.angleToVectorAboutNormal(coord->getX(),coord->getZ());
        VP0(( "gamma = %lf deg", gamma/DEGTORAD ));
	beta = acos(cosBeta);
        VP0(( "beta = %lf deg", beta/DEGTORAD ));
        VP0(( "Calculating alpha"));
        alpha = labX.angleToVectorAboutNormal(nodeNorm,labZ);
        VP0(("alpha= %lf deg", alpha/DEGTORAD ));
    }

	// Now calculate Phi and Theta

    translate = coord->getOrigin();
    VP0(("translate {%lf,%lf,%lf}", translate.getX(),translate.getY(),translate.getZ() ));
    distance = translate.length();
    VP0(( "distance = %lf", distance ));
    if ( distance < 0.00000001 ) {
        VP0(( "distance is zero" ));
	phi = 0.0;
	theta = 0.0;
    } else {
        VP0(( "distance is not zero" ));
	Vector3 tn = translate.normalized();
	double cosTheta = tn.dotProduct(labZ);
	VP0(("cosTheta = %lf", cosTheta ));
	Vector3 tnorm= labZ.crossProduct(tn);
	if ( fabs(tnorm.length()) < PARALLEL_CUTOFF ) {
	    VP0(( "tnorm.length is zero "));
	    theta = (cosTheta>=0.0)?0.0:(180.0*DEGTORAD);
	    phi = 0.0;
	} else {
	    VP0(( "tnorm.length is not zero "));
	    theta = acos(cosTheta);
	    phi = labX.angleToVectorAboutNormal(tnorm,labZ);
	}
	VP0(( "theta = %lf deg", theta/DEGTORAD ));
	VP0(( "phi = %lf deg", phi/DEGTORAD ));
    }
    VP0(( "Setting values"));
    this->_Alpha = alpha;
    this->_Beta = beta;
    this->_Gamma = gamma;
    this->_Distance = distance;
    this->_Theta = theta;
    this->_Phi = phi;
}

string	Dumb_XConventionEulerTransform::asString()
{
stringstream	ss;
    ss << "Alpha = " << this->_Alpha/0.0174533 << endl;
    ss << "Beta = " << this->_Beta/0.0174533 << endl;
    ss << "Gamma = " << this->_Gamma/0.0174533 << endl;
    ss << "Phi = " << this->_Phi/0.0174533 << endl;
    ss << "Theta = " << this->_Theta/0.0174533 << endl;
    ss << "Distance = " << this->_Distance << endl;
    return ss.str();
}

void	Dumb_XConventionEulerTransform::defineForCoordinateSystemToCoordinateSystem(RPCoordinateSystem start, RPCoordinateSystem dest)
{
Matrix	toCanonical;
RPCoordinateSystem	transformedDest;

    toCanonical = start->matrixToCanonical();
    transformedDest = dest->copy();
    transformedDest->transformWithMatrix(toCanonical);
    this->defineForCoordinateSystem(transformedDest);
}


RPQDomNode	Dumb_XConventionEulerTransform::asXml()
{
RPQDomNode	n;
    n = new_RPQDomNode("EulerTransform");
    n->addAttributeDouble("Alpha",this->_Alpha, 8, 5 );
    n->addAttributeDouble("Beta",this->_Beta, 8, 5 );
    n->addAttributeDouble("Gamma",this->_Gamma, 8, 5 );
    n->addAttributeDouble("Dist",this->_Distance, 8, 5 );
    n->addAttributeDouble("Phi",this->_Phi, 8, 5 );
    n->addAttributeDouble("Theta",this->_Theta, 8, 5 );
    return n;
}


RPCoordinateSystem	Dumb_XConventionEulerTransform::getCoordinateSystem()
{
Matrix		mt;
RPCoordinateSystem	c;

    c = new_RPCoordinateSystem();
    mt = this->matrixFromCanonical();
    c->transformWithMatrix(mt);
    return c;
}


Matrix	Dumb_XConventionEulerTransform::matrixFromCanonical()
{
_FUNCTION_TRACE("Dumb_XConventionEulerTransform::matrixFromCanonical");
    Matrix mD; mD.rightHandedRotationZ(this->_Alpha);
    Matrix mC; mC.rightHandedRotationX(this->_Beta);
    Matrix mB; mB.rightHandedRotationZ(this->_Gamma);
    Matrix mPhi; mPhi.rightHandedRotationZ(this->_Phi);
    Matrix mTheta; mTheta.rightHandedRotationX(this->_Theta);
    VP0(("mPhi = \n%s", mPhi.asXml()->asString().c_str() ));
    VP0(("mTheta = \n%s", mTheta.asXml()->asString().c_str() ));
    Vector3 vTrans; vTrans.set(0.0,0.0,this->_Distance);
    vTrans = mTheta.multiplyByVector3(vTrans);
    vTrans = mPhi.multiplyByVector3(vTrans);
    Matrix mTranslate; mTranslate.translate(&vTrans);
    VP0(("mTranslate = \n%s", mTranslate.asXml()->asString().c_str() ));
    Matrix mA = mC*mB;
    mA = mD*mA;
    VP0(("mA = \n%s", mA.asXml()->asString().c_str() ));
    mA = mTranslate*mA;
    VP0(("mTransform = \n%s", mA.asXml()->asString().c_str() ));
    return mA;
}


Matrix	Dumb_XConventionEulerTransform::matrixToCanonical()
{
RPCoordinateSystem	c;
    c = this->getCoordinateSystem();
    return c->matrixToCanonical();
}





//
//	CoordinateSet
//
//	Constructor
//
Dumb_CoordinateSystem::Dumb_CoordinateSystem()
{
    REF_CREATE("Dumb_CoordinateSystem");
    this->canonical();
}


Dumb_CoordinateSystem::Dumb_CoordinateSystem(const Dumb_CoordinateSystem& orig)
{
    REF_CREATE("Dumb_CoordinateSystem");
    this->origin = orig.origin;
    this->x = orig.x;
    this->y = orig.y;
    this->z = orig.z;
}


//
//	matrixFromCanonical
//
//	Generate the matrix for transformation
//	from the canonical coordinate system to this one
//
//	This means that a point at 0,0,0 will be transformed to
//	this coordinate systems origin.
//	A point at 1,0,0 will be transformed to a point
//	one unit along this ones X-axis
//
Matrix	Dumb_CoordinateSystem::matrixFromCanonical()
{
Matrix	mt, m;

    mt.atColRowPut( 0, 0, this->x.getX() );
    mt.atColRowPut( 0, 1, this->x.getY() );
    mt.atColRowPut( 0, 2, this->x.getZ() );

    mt.atColRowPut( 1, 0, this->y.getX() );
    mt.atColRowPut( 1, 1, this->y.getY() );
    mt.atColRowPut( 1, 2, this->y.getZ() );

    mt.atColRowPut( 2, 0, this->z.getX() );
    mt.atColRowPut( 2, 1, this->z.getY() );
    mt.atColRowPut( 2, 2, this->z.getZ() );

    m.translate(&(this->origin));
    m = m*mt;
    return m;
}


//
//	matrixToCanonical
//
//	Generate the matrix for transformation
//	to the canonical coordinate system from this one
//
//	This means that a point on this coordinate systems origin will be
//	transformed to 0,0,0.  A point one unit along this coordinate systems
//	X-axis will be transformed to 1,0,0
//
Matrix	Dumb_CoordinateSystem::matrixToCanonical()
{
Matrix  mt, m;
Vector3 vt;
    m.atColRowPut( 0, 0, this->x.getX() );
    m.atColRowPut( 1, 0, this->x.getY() );
    m.atColRowPut( 2, 0, this->x.getZ() );

    m.atColRowPut( 0, 1, this->y.getX() );
    m.atColRowPut( 1, 1, this->y.getY() );
    m.atColRowPut( 2, 1, this->y.getZ() );

    m.atColRowPut( 0, 2, this->z.getX() );
    m.atColRowPut( 1, 2, this->z.getY() );
    m.atColRowPut( 2, 2, this->z.getZ() );

    vt = this->origin.multiplyByScalar(-1.0);

    mt.translate(&vt);
    m = m*mt;
    return m;
}


//
//	canonical
//
//	Set (this) to the canonical coordinate set.
//	origin is 0,0,0
//	x is 1,0,0
//	y is 0,1,0
//	z is 0,0,1
void Dumb_CoordinateSystem::canonical()
{
    this->origin = Vector3(0.0,0.0,0.0);
    this->x = Vector3(1.0,0.0,0.0);
    this->y = Vector3(0.0,1.0,0.0);
    this->z = Vector3(0.0,0.0,1.0);
}


//
//	defineForAtoms
//
//	Build a coordinate set using the three atoms
//	the origin is on atom (aorigin)
//	the x axis is along (ax-aorigin)
//	the z axis is orthogonal to (axy) and x axis
//	the y axis is orthogonal to the new x and z axes
void	Dumb_CoordinateSystem::defineForAtoms( const RPAtom& aorigin, 
						const RPAtom& ax, 
						const RPAtom& axy )
{
_FUNCTION_TRACE("Dumb_CoordinateSystem::defineForAtoms");
Vector3	vo, vx, vxy, vz, vy;
    vo = aorigin->getPosition();
    VP0(( "vo=%lf,%lf,%lf",vo.getX(),vo.getY(),vo.getZ()));
    vx = (ax->getPosition() - vo).normalized();
    VP0(( "vx=%lf,%lf,%lf",vx.getX(),vx.getY(),vx.getZ()));
    vxy = (axy->getPosition() - vo).normalized();
    VP0(( "vxy=%lf,%lf,%lf",vxy.getX(),vxy.getY(),vxy.getZ()));
    vz = (vx.crossProduct(vxy)).normalized();
    VP0(( "vz=%lf,%lf,%lf",vz.getX(),vz.getY(),vz.getZ()));
    vy = (vz.crossProduct(vx)).normalized();
    VP0(( "vy=%lf,%lf,%lf",vy.getX(),vy.getY(),vy.getZ()));
    this->origin = vo;
    this->x = vx;
    this->y = vy;
    this->z = vz;
}


//
//	defineForVectorsOriginXDirYDir
//
//	Build a coordinate set using the origin and the direction
//	of the X-axis and Y-axis.  The Z-axis is calculated from
//	the cross-product of xDir and yDir.
//
void	Dumb_CoordinateSystem::defineForVectorsOriginXDirYDir( 
					const Vector3& orig,
					  const Vector3& xDir,
					  const Vector3& yDir )
{
Vector3	vzt;
    this->origin = orig;
    this->x = xDir.normalized();
    this->y = yDir.normalized();
    vzt = (this->x).crossProduct(this->y);
    this->z = vzt.normalized();
}


//
//	defineForVectorsOriginXDirXYPlane
//
//	Build a coordinate set using the origin and the direction
//	of the X-axis and a vector in the XY plane.  
//	The Z-axis is calculated from the cross-product of xDir and xyPlane.
//
void	Dumb_CoordinateSystem::defineForVectorsOriginXDirXYPlane( 
					const Vector3& orig,
					  const Vector3& xDir,
					  const Vector3& xyPlane )
{
Vector3	vzt,vxy,vyt;
    this->origin = orig;
    this->x = xDir.normalized();
    vxy = xyPlane.normalized();
    vzt = (this->x).crossProduct(vxy);
    this->z = vzt.normalized();
    vyt = (this->z).crossProduct(this->x);
    this->y = vyt.normalized();
}

//
//	defineForVectorsOriginZDirXZPlane
//
//	Build a coordinate set using the origin and the direction
//	of the Z-axis and a vector in the XZ plane.  
//	The Y-axis is calculated from the cross-product of xDir and xyPlane.
//
void	Dumb_CoordinateSystem::defineForVectorsOriginZDirXZPlane( 
					const Vector3& orig,
					  const Vector3& zDir,
					  const Vector3& xzPlane )
{
Vector3		vyt,vxz,vxt;
    this->origin = orig;
    this->z = zDir.normalized();
    vxz = xzPlane.normalized();
    vyt = (this->z).crossProduct(vxz);
    this->y = vyt.normalized();
    vxt = (this->y).crossProduct(this->z);
    this->x = vxt.normalized();
}




//
//	defineForVectorsOriginXDirZDir
//
//	Build a coordinate set using the origin and the direction
//	of the X-axis and Z-axis.  The Y-axis is calculated from
//	the cross-product of xDir and zDir.
//
void	Dumb_CoordinateSystem::defineForVectorsOriginXDirZDir( 
					const Vector3& orig,
					  const Vector3& xDir,
					  const Vector3& zDir )
{
Vector3	vyt;
    this->origin = orig;
    this->x = xDir.normalized();
    this->z = zDir.normalized();
    vyt = (this->z).crossProduct(this->x);
    this->y = vyt.normalized();
}


void	Dumb_CoordinateSystem::defineRandom( )
{
double x,y,z;
#define	RANGE	20.0
    x = (rand()/(RAND_MAX+1.0))*RANGE - (RANGE/2.0);
    y = (rand()/(RAND_MAX+1.0))*RANGE - (RANGE/2.0);
    z = (rand()/(RAND_MAX+1.0))*RANGE - (RANGE/2.0);
    this->origin.set(x,y,z);
    while (1) {
	x = (rand()/(RAND_MAX+1.0))*2.0 - 1.0;
	y = (rand()/(RAND_MAX+1.0))*2.0 - 1.0;
	z = (rand()/(RAND_MAX+1.0))*2.0 - 1.0;
	this->x.set(x,y,z);
	if ( this->x.length() > 0.00001 ) break;
    }
    this->x = this->x.normalized();
    while (1) {
	x = (rand()/(RAND_MAX+1.0))*2.0 - 1.0;
	y = (rand()/(RAND_MAX+1.0))*2.0 - 1.0;
	z = (rand()/(RAND_MAX+1.0))*2.0 - 1.0;
	this->y.set(x,y,z);
	if ( this->y.length() > 0.00001 ) break;
    }
    this->z = this->x.crossProduct(this->y);
    this->z = this->z.normalized();
    this->y = this->z.crossProduct(this->x);
}






//
//	matrixForTransformTo
//
//	Return the matrix to transform from (this) to (cs)
Matrix	Dumb_CoordinateSystem::matrixForTransformTo(const RPCoordinateSystem& cs )
{
Matrix	m1, m2;

    m1 = this->matrixToCanonical();
    m2 = cs->matrixFromCanonical();
    return(m1*m2);
}




//
//	transformWithMatrix
//
//	Transform this coordinate system with the matrix
//
void	Dumb_CoordinateSystem::transformWithMatrix( Matrix& m )
{
Vector3	vox, voy, voz;

    vox = m*(this->x + this->origin);
    voy = m*(this->y + this->origin);
    voz = m*(this->z + this->origin);
    this->origin = m*this->origin;
    this->x = vox - this->origin;
    this->y = voy - this->origin;
    this->z = voz - this->origin;
}


//
//	renderXml
//
//	Render the coordinate system in XML format
//
RPQDomNode	Dumb_CoordinateSystem::renderXml( double width, 
							double axisLength)
{
RPQDomNode	graalphacs, line;

    graalphacs = xmlBlock();
    line = xmlLine( XML_RED, (int)width, this->origin, 
				this->origin+this->x*axisLength );
    graalphacs->addChild(line);
    line = xmlLine( XML_GREEN, (int)width, this->origin, 
				this->origin+this->y*axisLength );
    graalphacs->addChild(line);
    line = xmlLine( XML_BLUE, (int)width, this->origin, 
				this->origin+this->z*axisLength );
    graalphacs->addChild(line);
    return graalphacs;
}




//
//	dump
//
//	Dump the coordinate system
//
void	Dumb_CoordinateSystem::dump()
{
    printf( "origin: ( %lf, %lf, %lf )\n", 
	this->origin.getX(), this->origin.getY(), this->origin.getZ() );
    printf( "x-axis: ( %lf, %lf, %lf )\n", 
	this->x.getX(), this->x.getY(), this->x.getZ() );
    printf( "y-axis: ( %lf, %lf, %lf )\n", 
	this->y.getX(), this->y.getY(), this->y.getZ() );
    printf( "z-axis: ( %lf, %lf, %lf )\n", 
	this->z.getX(), this->z.getY(), this->z.getZ() );
}

RPQDomNode	Dumb_CoordinateSystem::asXml()
{
RPQDomNode	node,vec;
    node = new_RPQDomNode("CoordinateSystem");
    node->addChild(this->origin.asXml("Origin"));
    node->addChild(this->x.asXml("XDir"));
    node->addChild(this->y.asXml("YDir"));
    node->addChild(this->z.asXml("ZDir"));
    return node;
}


RPCoordinateSystem	Dumb_CoordinateSystem::copy()
{
RPCoordinateSystem	n;
    n = new_RPCoordinateSystem(shared_from_this());
    return n;
}

    
bool	Dumb_CoordinateSystem::sameAs(RPCoordinateSystem c)
{
    if ( !this->origin.sameAs(c->getOrigin()) ) return false;
    if ( !this->x.sameAs(c->getX()) ) return false;
    if ( !this->y.sameAs(c->getY()) ) return false;
    if ( !this->z.sameAs(c->getZ()) ) return false;
    return true; 
}

    

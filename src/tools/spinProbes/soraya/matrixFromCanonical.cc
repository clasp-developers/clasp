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



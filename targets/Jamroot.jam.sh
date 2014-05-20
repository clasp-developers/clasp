# nothing

echo "use-project externals : build/$DEVELOPMENT_ENVIRONMENT/lib ;"
echo ""
echo "project : requirements"
echo "          <toolset>gcc:<include>build/$DEVELOPMENT_ENVIRONMENT/include/boost-$BOOST_VERSION"
echo "          <toolset>gcc:<include>build/$DEVELOPMENT_ENVIRONMENT/include"
echo "          <toolset>darwin:<include>build/$DEVELOPMENT_ENVIRONMENT/include/boost-$BOOST_VERSION"
echo "          <toolset>darwin:<include>build/$DEVELOPMENT_ENVIRONMENT/include"
echo "  	<include>src"
echo "   ; "








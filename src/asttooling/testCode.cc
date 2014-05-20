#include <stdio.h>
#include <core/foundation.h>
#include <core/object.h>
#include <core/holder.h>


namespace asttooling {


    void tinyFunc()
    {

        vector<std::string> dummyStrings;

        dummyStrings.push_back("This");
        dummyStrings.push_back("is");
        dummyStrings.push_back("a");
        dummyStrings.push_back("test");

        //! mytest
        for ( vector<std::string>::const_iterator it=dummyStrings.begin(); it!=dummyStrings.end(); ++it )
        {
            std::cout << *it << " ";
        }
        std::cout << std::endl;
    }

};



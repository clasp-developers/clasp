#ifndef character_fwd_H
#define character_fwd_H
namespace core
{
FORWARD(Character);
FORWARD(BaseChar);
FORWARD(StandardChar);
FORWARD(ExtendedChar);




    struct CharacterInfo  {
        map<string,int>                 gNamesToCharacterIndex;
        gctools::Vec0<Character_sp>     gIndexedCharacters;
        gctools::Vec0<Str_sp>		gCharacterNames;
        const char* repr() const { return "CharacterInfo";};
	CharacterInfo(); 
    };


}
#endif

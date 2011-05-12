% vcard.hrl : record and macro definitions for vcard handling

-record(vcardproperty,{name,value_type,types}).
-record(vcardkey,{name,value,types}).

-define(BEGIN_VCARD,[#vcardkey{name="BEGIN",value="VCARD",types=[]},
		     #vcardkey{name="VERSION",value="2.0",types=[]}]).

-define(END_VCARD,[#vcardkey{name="END",value="VCARD",types=[]}]).

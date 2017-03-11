unit LecReg;

interface

procedure Register;

implementation
uses
  Classes, DesignIntf, Dbsearch, Txtsrch, Pikflds, Dbslocat, Parser, Calc;

procedure Register;
begin
  RegisterComponents( 'Lec', [TdbSearch, TTextSearch] );
  RegisterComponentEditor(TdbSearch, TdbSearchEditor);
  RegisterPropertyEditor( TypeInfo(string), TdbSearch, 'DefSearchField', TDefSrchFldProperty );
  RegisterPropertyEditor( TypeInfo( TStrings ), TdbSearch, 'SearchFields', TSrchFldsProperty );
end;

end.

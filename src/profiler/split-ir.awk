BEGIN { filename = "prelude.txt"; }
/^\*\*\* IR Dump/ { BEFORE_AFTER = $4;
    STAGE = $5;
    gsub("[/]","-",STAGE);
    next; }
/^define[^@]*@/ {
  close(filename);
  atsign = match($0,"@");
  rest = substr($0, atsign, length($0)-atsign);
  openp = match(rest,"[(]");
  front = substr(rest,0,openp-1);
  gsub("/","-",front);
    filename = front "-" BEFORE_AFTER "-" STAGE;
    print "filename = ", filename;
    print $0 >> filename;  }
// { print $0 >> filename; }

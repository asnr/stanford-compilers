**)

(*Yep "comment" can't get -- out easily **)

class CellularAutomaton inherits IO {-- yep this *) be comment
-- more comment -- even more
class Main {----
    cells : CellularAutomaton;
   
    main() : SELF_TYPE {
        {
            cells <- (new CellularAutomaton).init("SHOULD RETURN STRING TOO LONG ERROR, lorem ipsum you know the rest adipiscing elit.\
 Maecenas at consectetur leo, sit amet mollis erat. Nulla rhoncus ante quis purus malesuada viverra. Nam non tortor in elit \
condimentum lacinia. Vivamus condimentum, leo id porta iaculis, est sapien congue dolor, et varius arcu magna in risus. Mauris \
hendrerit magna justo, vitae pharetra elit ullamcorper non. Aliquam tempus, ex in scelerisque faucibus, tellus turpis dictum nibh, id \
malesuada turpis neque in est. Phasellus ac risus augue. Praesent semper, lorem ut lobortis cursus, ante tortor condimentum ex, sit amet \
 sem quam vitae augue. Aliquam et tincidunt lorem. Duis ullamcorper vulputate libero et laoreet.\
\
Aliquam lobortis egestas sem, id efficitur neque tristique sed. Etiam tristique felis vel lectus ornare dignissim. Ut in purus quis \
lorem feugiat pellentesque quis ac felis. Proin ut lacinia tortor, nec aliquam massa. Etiam at aliquet ante, a sagittis lorem. Nam quis \
enim lacus. Proin quis malesuada lacus. Pellentesque tempor lorem id ligula bibendum, ut vestibulum diam maximus. Nam bibendum ante \
neque, ut tempor arcu ullamcorper consectetur. Nullam ac risus condimentum, sodales nisi aliquet, rutrum orci. Donec cursus feugiat mi, \
in sollicitudin mauris ullamcorper vel. Suspendisse viverra mi sit amet mi tempor commodo. Donec hendrerit augue sit amet consequat\
laoreet.");
            cells.print();
            (let countdown : Int <- 20 in
                while countdown > 0 loop
                    {
                        cells.evolve();
                        cells.print();
                        countdown <- countdown - 1;
                    
                pool
             end let countdown);
            self;
        }
    };
};

" oh noes EOF...
Employee:([]id:1 2 3 4 5 6 7;Name: `John`Mike`Sally`Jane`Joe`Dan`Phil; salary:300 200 550 500 600 600 550; manager_id:3 3 4 7 7 3 0)

/ Employee query
select Name from (Employee lj ([manager_id:Employee`id] wage:Employee`salary)) where (not null manager_id) and (salary>wage)
select avg salary from Employee where (not null manager_id) and (not id in Employee`manager_id)

/Exists?
exists:{x in key `.}

/Dictionary to list.
undict : {:$[99h=type x;(key x;.z.s each value x);x]} 

/Infinite loop.
{sqrt x}\[{x>0};100]

/Depends on me.
dependson:{if[doesExist x; .z.s each .z.b 0N! x;]}

/Who moved?
v:10 20 30 40 50 60 70
w:10 20 70 30 40 50 60
moved :{first where ((y?x) - (til count y))=1}

/Maximum Overlap Intervals
/ (1)
len: count procs;
procs: update head:start>=(len,len)#end, tail:end<=(len,len)#start, class:anest=(len,len)#anest from procs;
procs: update w:(class * (head+tail)=0)=1 from procs;
procs: update w:id @[w;::;where] from procs;
procs: select id,anest,start,end,w from procs;

/ (2)
anestList: distinct procs`anest;
sList: ();
iAnest: 0;
while[iAnest<count anestList;
    target: anestList[iAnest];
    tb: select from procs where anest=target;
    timeList: tb`start;
    timeList,: tb`end;
    len: -1 + count timeList;
    tb: update head: start<=((count tb),len)#(timeList til len) from tb;
    tb: update tail: end>=((count tb),len)#(1+timeList til len) from tb;
    tb: update contained: head*tail from tb;
    tb: update overlap: contained*((count tb),len)#(sum contained) from tb;
    tb: update s: @[overlap;::;max] from tb;
    sList,: tb`s;
    iAnest: iAnest+1];
procs: update s:sList from procs;


/Pascal Triangle
pascal:{{prior[+]x,0}\[x-1;1]}
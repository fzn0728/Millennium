Employee:([]id:1 2 3 4 5 6 7;Name: `John`Mike`Sally`Jane`Joe`Dan`Phil; salary:300 200 550 500 600 600 550; manager_id:3 3 4 7 7 3 3)


/Exists?
exists : {$['x=type x;1b;0b]}

/Dictionary to list.
undict : {:$[99h=type x;(key x;.z.s each value x);x]}

/Infinite loop.
/ {sqrt x}\[{x>0};100]

/Depends on me.
dependson:{.z.b x}

/Who moved?
v:10 20 30 40 50 60 70
w:10 20 70 30 40 50 60
moved :{first where ((y?x) - (til count y))=1}




new: Employee lj Employee
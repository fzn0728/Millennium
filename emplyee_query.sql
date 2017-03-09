select distinct Name from Employee A Join Employee B on A.manager_id = B.id WHERE A.salary > B.salary

select avg（salary） from Employee where id not in （select distinct manager_id from employee where manager_id != NULL）
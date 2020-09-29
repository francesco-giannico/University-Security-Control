drop table person;
drop table dept;
drop table deptextclosing;
drop table buildextclosing;
drop table building;
drop table pidreader;
drop table entrance;
drop table evalidation;
drop table pidcard;
drop table hourminutes;
drop table extclosing;
drop table personremoved;
drop table error_entrance;
drop type evalidationty force;
drop type pidreaderty force;
drop type pidcardty force;
drop type ref_buildingallowednt force;
drop type buildingallowedty force;
drop type buildingty force;
drop type deptty force;
drop type employeety force;
drop type studentty force;
drop type personty force;
drop type entrancety force;
drop type buildextclosingty force;
drop type deptextclosingty force;
drop type hourminutesty force;
drop sequence pidcard_seq;
drop sequence pidreader_seq;
drop sequence building_seq;
drop sequence evalidation_seq;
drop procedure populate_database;
drop procedure alter_all_triggers;
drop procedure populate_hourminutes;
drop procedure populate_depts;
drop procedure populate_persons;
drop procedure populate_buildings;
drop procedure populate_pidreaders;
drop procedure populate_extclosings;
drop procedure populate_deptextclosings;
drop procedure populate_buildextclosings;
drop procedure populate_pidcards;
drop procedure populate_entrancesevalidations;
/*
drop index index_evalidation;
drop index index_entrance;*/
/*drop index idx_nameperson;
drop index idx_surnameperson;*/

--TYPES
create or replace type hourminutesty as object(
    hours number,
    minutes number
);
/
create or replace type deptty as object(
 namedept varchar2(30),
 openat  ref hourminutesty,
 closeat ref hourminutesty
);
/
create or replace type buildingty as object(
      code integer,
      address varchar2(30),
      nameb varchar2(30),
      openat ref hourminutesty,
      closeat ref hourminutesty,
      dept ref deptty,
      constructor function buildingty(self in out nocopy buildingty,nameb varchar2,address varchar2,openat ref hourminutesty,closeat ref hourminutesty,dept ref deptty) return self as result
);
/
create or replace type body buildingty is
 constructor function buildingty(self in out nocopy buildingty,nameb varchar2,address varchar2,openat ref hourminutesty,closeat ref hourminutesty,dept ref deptty) return self as result is
	begin
		self.nameb:=nameb;
    self.address:=address;
    self.openat:=openat;
    self.closeat :=closeat;
    self.dept:=dept;
		return;
	end;
end;
/

create or replace type personty as object(
  namep varchar2(30),
  surname varchar2(30),
  fiscalcode char(16),
  dateofbirth date,
  dept ref deptty,
  constructor function personty(self in out nocopy personty, namep varchar2, surname varchar2, fiscalcode char,dateofbirth date,dept ref deptty) return self as result
) not final;
/
create or replace type body personty is
	constructor function personty(self in out nocopy personty, namep varchar2, surname varchar2, fiscalcode char,dateofbirth date,dept ref deptty) return self as result is
	begin
		self.namep := namep;
		self.surname := surname;
		self.fiscalcode := fiscalcode;
    self.dateofbirth := dateofbirth;
    self.dept:=dept;
		return;
	end;
end;
/
create or replace type employeety under personty(
  contractID varchar2(15),
  profession varchar2(30), 
  enddate_workcontract date,
  constructor function employeety(self in out nocopy employeety,namep varchar2, surname varchar2, fiscalcode char,dateofbirth timestamp,dept ref deptty, contractID varchar2,profession varchar2, enddate_workcontract date) return self as result
);
/
create or replace type body employeety is
	constructor function employeety(self in out nocopy employeety,namep varchar2, surname varchar2, fiscalcode char,dateofbirth timestamp,dept ref deptty, contractID varchar2,profession varchar2, enddate_workcontract date) return self as result is
	begin
  
  	self.namep := namep;
		self.surname := surname;
		self.fiscalcode := fiscalcode;
    self.dateofbirth := dateofbirth;
    
    self.dept:=dept;
		self.contractID:=contractID;
    self.profession:=profession;
    self.enddate_workcontract:=enddate_workcontract;
		return;
	end;
end;
/
create or replace type studentty under personty(
  badgenumber char(6),
  status char(1),
  course varchar2(30),
  academicyear varchar2(10),
  constructor function studentty(self in out nocopy studentty,namep varchar2, surname varchar2, fiscalcode char,dateofbirth timestamp,dept ref deptty, badgenumber char, course varchar2, academicyear varchar2) return self as result
);
/
create or replace type body studentty is
	constructor function studentty(self in out nocopy studentty,namep varchar2, surname varchar2, fiscalcode char,dateofbirth timestamp,dept ref deptty,badgenumber char, course varchar2, academicyear varchar2) return self as result is
	begin
  
  	self.namep := namep;
		self.surname := surname;
		self.fiscalcode := fiscalcode;
    self.dateofbirth := dateofbirth;
    
    self.dept:=dept;
		self.badgenumber := badgenumber;
		self.course := course;
		self.academicyear := academicyear;
    --the following is setted by default
		self.status :='A';
		return;
	end;
end;
/
create or replace type buildingallowedty as object(
    building ref buildingty
);
/
create or replace type ref_buildingallowednt as table of buildingallowedty;
/
create or replace type pidcardty as object(
  pidcardID integer,
  status char(1),
  releasedate date,
  person char(16),
  buildingallowed ref_buildingallowednt,
  special_card char(1),
  constructor function pidcardty(self in out nocopy pidcardty,person char,buildingallowed ref_buildingallowednt) return self as result
);
/
create or replace type body pidcardty is
	constructor function pidcardty(self in out nocopy pidcardty,person char,buildingallowed ref_buildingallowednt) return self as result is
	begin
    self.person := person;
    self.releasedate := sysdate;
    self.status := 'A';
    self.special_card:='F';
    self.buildingallowed:=buildingallowed;
		return;
	end;
end;
/
create or replace type pidreaderty as object(
  pidreaderID integer,
  building ref buildingty,
  constructor function pidreaderty(self in out nocopy pidreaderty,building ref buildingty) return self as result
);
/
create or replace type body pidreaderty is
	constructor function pidreaderty(self in out nocopy pidreaderty,building ref buildingty) return self as result is
	begin
    self.building:=building;
		return;
	end;
end;
/
create or replace type evalidationty as object(
  datehour timestamp,
  pidcard ref pidcardty,
  pidreader ref pidreaderty,
  idevalidation integer,
  constructor function evalidationty(self in out nocopy evalidationty,timehour timestamp,pidcard ref pidcardty, pidreader ref pidreaderty) return self as result,
  constructor function evalidationty(self in out nocopy evalidationty,pidcard ref pidcardty, pidreader ref pidreaderty) return self as result
);
/
create or replace type body evalidationty is
          constructor function evalidationty(self in out nocopy evalidationty, timehour timestamp,pidcard ref pidcardty, pidreader ref pidreaderty) return self as result is
          begin
                self.pidcard := pidcard;
                self.pidreader :=pidreader;
                self.datehour:=timehour;
                return;
          end;
           constructor function evalidationty(self in out nocopy  evalidationty,pidcard ref pidcardty, pidreader ref pidreaderty) return self as result is
          begin
                self.pidcard := pidcard;
                self.pidreader :=pidreader;
                self.datehour := localtimestamp;
                return;
          end;
end;
/
create or replace type entrancety as object(
  evalidation integer,
  building ref buildingty,
  datehour timestamp
);
/
create or replace type extclosingty as object(
  startat ref hourminutesty,
  endat ref hourminutesty,
  datetime date
);
/
create or replace type buildextclosingty as object (
  building ref buildingty,
  extclosing ref extclosingty
);
/
create or replace type deptextclosingty as object (
  deptextclosing ref deptty,
  extclosing ref extclosingty
);
/
--TABLES
create table error_entrance (
  message varchar2(100)
);
/
create table hourminutes of hourminutesty(
  primary key(hours,minutes),
  CHECK (hours>=0 and hours<=23),
  CHECK(minutes>=0 and  minutes<=59)
);
/
create table person of personty(
  primary key(fiscalcode)
);
/
create table dept of deptty(
  primary key(namedept)
);
/
create table extclosing of extclosingty;
/
create table deptextclosing of deptextclosingty;
/
create table buildextclosing of buildextclosingty;
/
create table building of buildingty (
  primary key(code)
);
/
create table pidreader of pidreaderty(
  primary key(pidreaderID)
);
/
create table entrance of entrancety;
/
create table evalidation of evalidationty;
/
create table pidcard of pidcardty(
 primary key(pidcardID)
) nested table buildingallowed store as buildingallowedNT;
/
create table personremoved of personty;
/
--sequence
create sequence pidcard_seq start with 1;
/
create sequence pidreader_seq start with 1;
/
create sequence building_seq start with 1;
/
create sequence evalidation_seq start with 1;
/

--index
/*create   index index_evalidation
on evalidation(datehour);
/
create   index index_entrance
on entrance(datehour);
/*/
create index  idx_nameperson on person(namep);
/
create index idx_surnameperson on person(surname);
/
--TRIGGERS
create or replace trigger evalidation_incr
before insert on evalidation
for each row
begin
 select evalidation_seq.nextval into :new.idevalidation
  from dual;
end;
/
create or replace trigger building_incr 
before insert on building
for each row
begin
 select building_seq.nextval into :new.code
  from dual;
end;
/
create or replace trigger pidreader_incr
before insert on pidreader
for each row
begin
 select pidreader_seq.nextval into :new.pidreaderID
  from dual;
end;
/
create or replace trigger pidcard_incr
before insert on pidcard
for each row
begin
 select pidcard_seq.nextval into :new.pidcardID
  from dual;
end;
/

--EVALIDATION AND ENTRANCE
--for efficiency i have set this trigger as "after". It could be before but since it doesn't modify the new referencing it is not important 
create or replace trigger checkduplies_evalidation
before insert on evalidation
for each row
declare
new_row evalidationty;
evalidationexist integer;
begin
 new_row := :new.sys_nc_rowinfo$;
  --check if there is a duplied evalidation 
  select count(*) into evalidationexist from evalidation e where e.datehour=new_row.datehour and e.pidcard=new_row.pidcard and e.pidreader=new_row.pidreader;
  if evalidationexist>0 then
         raise_application_error(-20001, 'this evalidation already exists!');
  end if;
end;
/
--di tipo after perchè dopo aver aggiunto l'evalidation(vengono tutti registrati,ricorda) se è valido allora devi aggiungere una tupla in entrance!
create or replace trigger check_evalidation
after insert on evalidation
for each row
declare
pidcardactive number;
allowed number;
isopen number;
extclosingdates number;
untilhour number;
new_row evalidationty;
errore varchar2(100);
x integer;
rff_build ref buildingty;
evalidationexist integer;
enddate date;
is_special integer;
is_employee integer;
begin
      --check if the pidcard is active
      select count(*) into pidcardactive from pidcard p where :new.pidcard= ref(p) and status='A';
      if pidcardactive>0 then
               --   if it is an employee check if the contract is not ended.
                  enddate:=null;
                  select count(*) into is_employee 
                  from person f
                   where (value(f) is of type (employeety)) and deref(:new.pidcard).person=fiscalcode;
                   if is_employee>0 then 
                     select treat(value(f) as employeety).enddate_workcontract into enddate 
                     from person f
                     where (value(f) is of type (employeety)) and deref(:new.pidcard).person=fiscalcode;
                   end if;
                    if enddate<>null and enddate>current_date then
                          errore:='the work contract is ended so the card is now invalid!';
                          --invalid the pidcard
                          update pidcard
                          set status='D'
                          where deref(:new.pidcard).person=person;
                         raise_application_error(-20001, errore);
                     else 
                            --check if the pidreader is of the one of the allowed structures
                           select count(*) into allowed from pidreader p where ref(p)=:new.pidreader
                               and p.building in (select t.building as building from pidcard p,table(p.buildingallowed) t where :new.pidcard= ref(p));
                             if allowed>0 then
                                  --check if it is a special card
                                  extclosingdates:=0;
                                  select count(*) into is_special from pidcard p where ref(p)=:new.pidcard and special_card='T';
                                   if is_special=0 then 
                                        --check if the building is open at this hour
                                         select count(*) into isopen from building b where deref(:new.pidreader).building=ref(b)
                                          and to_number(extract(hour from :new.datehour)) 
                                                between (deref(b.openat).hours) and (deref(b.closeat).hours);
                              
                                           if isopen>0 then
                                            --check if is open on this date and at this hour
                                              select count(b.extclosing) into extclosingdates
                                              from buildextclosing b 
                                              where deref(:new.pidreader).building=b.building
                                               and  trunc(:new.datehour,'j')=deref(b.extclosing).datetime
                                               and to_number(extract(hour from :new.datehour)) <= (deref(deref(b.extclosing).endat).hours);
                                              
                                           else
                                                 errore:='this building is closed at this hour';
                                                 raise_application_error(-20001,errore);
                                          end if;
                                      end if;
                                       --today isn't a special closing day or you have a special card!
                                       if extclosingdates=0 then
                                       --INSERT INTO ENTRANCE!, GREAT!
                                        select evalidation_seq.currval into x from dual;
                                        dbms_output.put_line('entrance recorded!');
                                        select ref(b) into rff_build from building b where ref(b)=deref(:new.pidreader).building;
                                        --non mettere mai deref() in un insert, non verrà aggiunta la tupla tacitamente!!
                                        insert into entrance values (entrancety(new_row.idevalidation,rff_build,:new.datehour));
                                     else
                                       errore:='today this building is closed';
                                         raise_application_error(-20001, errore);
                                     end if;
                                 
                            else 
                               errore:='this pid card is not authorized to access in this building!';
                               raise_application_error(-20001, errore);
                            end if;   
                    end if;
           else 
             errore:='the pid card is not active,you cannot access in this building!';
             raise_application_error(-20001, errore);
        end if;
    

  --capturing exception we can allow the evalidation storing attempts! but no entrance is stored!
  exception
  when NO_DATA_FOUND then
    dbms_output.put_line('no data');
     insert into error_entrance values ('no data');
    when others then
      dbms_output.put_line(errore);
       insert into error_entrance values (errore);
end;
/
--PERSON
create trigger delete_person
after delete on person
for each row
declare
--student
fiscalcode char(16);
badgenumber char(6);
academicyear varchar2(10);
course varchar2(30);
status char(1);
--employee
profession varchar2(30);
contractid varchar2(15);
enddate date;
old_row personty;
begin
  old_row := :old.sys_nc_rowinfo$;
  badgenumber:=null;
  select treat(old_row as studentty).badgenumber into badgenumber from dual;
  if badgenumber<>null then 
  --recover other attributes
     select treat(old_row as studentty).course into course from dual;
     select treat(old_row as studentty).status into status from dual;
    select treat(old_row as studentty).academicyear into academicyear from dual;
     insert into personremoved values (studentty(old_row.namep,old_row.surname,old_row.fiscalcode,old_row.dateofbirth,old_row.dept,badgenumber,course,academicyear));
 
  else
  --is an employee
   select treat(old_row as employeety).enddate_workcontract into enddate from dual;
    select treat(old_row as employeety).contractID into contractID from dual;
    select treat(old_row as employeety).profession into profession from dual;
    insert into personremoved values (employeety(old_row.namep,old_row.surname,old_row.fiscalcode,old_row.dateofbirth,old_row.dept,contractID,profession,enddate));
  end if;
end;
/
create or replace trigger check_studentstatus
after update on person
for each row
declare
status char(1);
new_row personty;
begin
  status:=null;
   new_row := :new.sys_nc_rowinfo$;
  select treat(new_row as studentty).status into status from dual;
  if status<>null  and status='D' then 
      update pidcard
      set status='D'
      where :new.fiscalcode=person;
  end if;
end;
/
create or replace trigger check_basicinformation
before insert on person
for each row
declare
enddate date;
new_row personty;
duplies integer;
badgenumber char(6);
pragma autonomous_transaction;
begin
   new_row := :new.sys_nc_rowinfo$;
   enddate:=null;
    --the birthdate became 19 years before today.
  if new_row.dateofbirth>add_months(trunc(current_date),-12*18) then 
     raise_application_error(-20001, 'this person cannot be recorded into the system due to the age');
  end if;
  --if it is a student
  badgenumber:=null;
  select treat(new_row as studentty).badgenumber into badgenumber from dual;
  if badgenumber<>null then 
  --check if there is a duplicate key of badgenumber
    select count(*) into duplies 
    from person p 
    where (value(p) is of type (studentty)) and treat(value(p) as studentty).badgenumber=badgenumber;
    if duplies>0 then
       raise_application_error(-20001, 'this student already exists');
    end if;
  else
    --if it is an employee
    select treat(new_row as employeety).enddate_workcontract into enddate from dual;
     --the contract is still valid
    if  enddate<current_date then
         raise_application_error(-20001, 'the end work date must be after today');
      else 
         --check if it is a duplied contractID ( for example different fiscal code but equal contract id)
          select count(*) into duplies 
          from person p 
          where (value(p) is of type (employeety)) and treat(value(p) as employeety).contractid= treat(new_row as employeety).contractid;
          if duplies>0 then
             raise_application_error(-20001, 'this contractid already exists');
           end if;
    end if;
 end if;
end;
/
create or replace trigger generate_pidcard
after insert on person
for each row
declare
new_row personty;
begin
   new_row := :new.sys_nc_rowinfo$;
   --set the building associated to the department to which the person belongs to.
   insert into pidcard values (pidcardty(new_row.fiscalcode,(cast(multiset(select ref(b) from building b where b.dept=new_row.dept) as ref_buildingallowednt))));
end;
/
create or replace trigger deactive_pidcard
after insert on personremoved
for each row
begin
 update pidcard 
 set status='D' 
 where :new.fiscalcode=person;
end;
/
--check if openat < closeat  for build
create or replace trigger check_openclosebuild
after insert on building
for each row
declare
is_invalid integer;
begin
  select deref(:new.openat).hours into is_invalid
  from dual 
  where deref(:new.openat).hours> deref(:new.closeat).hours;
  if is_invalid>0 then 
     raise_application_error(-20001, 'the open hour is after the closing!');
  end if;
  
  --check if openhour is after or equal the open hour of the department and the closinghour is  before or equal the closinghour of the dept
  select count(*) into is_invalid from dept d where :new.dept=ref(d) 
      and deref(:new.openat).hours<=deref(openat).hours and deref(:new.closeat).hours>=deref(closeat).hours;
      
   if is_invalid>0 then 
     raise_application_error(-20001, 'the opening and closing hours are not synchronous with the department associated');
  end if;
end;
/
--creates a pidreader associated to the building
create or replace trigger generate_pidreader
after insert on building
for each row
follows check_openclosebuild
declare
bb ref buildingty;
pragma autonomous_transaction;
begin
  --recovering the ref of the new tuple
  select ref(b) into bb from building b where :new.code=code;
  insert into pidreader values  (pidreaderty(bb));
end;
/
--check if openat < closeat  for dept
create or replace trigger check_openclosedept
after insert on dept
for each row
declare
is_invalid integer;
begin
  select deref(:new.openat).hours into is_invalid
  from dual 
  where deref(:new.openat).hours> deref(:new.closeat).hours;
  if is_invalid>0 then 
     raise_application_error(-20001, 'the open hour is after the closing!');
  end if;
end;
/
--if there is a special date of closure for a department it will be the same for all the buildings associated to it
create or replace trigger extclosing
after insert on deptextclosing
for each row
declare
cursor cs is select ref(b) from building b where dept=:new.deptextclosing;
rf_build ref buildingty;
begin
  open cs;
  loop 
  fetch cs into rf_build;
    insert into buildextclosing values (buildextclosingty(rf_build,:new.extclosing));
  end loop;
  close cs;
end;
/
--PROCEDURE
create or replace procedure populate_hourminutes as
minutes number;
hours number;
begin
  hours:=-1;
  loop 
  hours:=hours+1;
    minutes:=-1;
    loop 
     minutes:= minutes+1;
     insert into hourminutes values (hourminutesty(hours,minutes));
     exit when minutes=59;
    end loop;
    exit when hours=23;
  end loop;
end;
/
create or replace procedure populate_depts as
iterations number;
from9_0 ref hourminutesty;
to19_0 ref hourminutesty;
begin
  iterations := 0;
  select ref(r) into from9_0 from hourminutes r where hours=9 and minutes=0;
  select ref(r) into to19_0 from hourminutes r where hours=19 and minutes=0;
  loop 
    insert into dept values (deptty(dbms_random.string('l',10),from9_0,to19_0));
    iterations := iterations + 1;
    exit when iterations = 30;
  end loop;
end;
/

create or replace procedure populate_buildings as
iterations number;
dept ref deptty;
openat ref hourminutesty;
closeat ref hourminutesty;
begin
iterations:=0;
    loop
        select ref_dept into dept
        from (select ref(d) as ref_dept from dept d order by dbms_random.value)
        where rownum=1;
        
        select ref(h) into openat from hourminutes h where minutes=deref(deref(dept).openat).minutes and  hours=deref(deref(dept).openat).hours;
        select ref(h) into closeat from hourminutes h  where minutes=deref(deref(dept).closeat).minutes and  hours=deref(deref(dept).closeat).hours;
        
        insert into building values (buildingty(dbms_random.string('x',10),dbms_random.string('x',10),openat,closeat,dept));
        iterations:=iterations+1;
        exit when iterations=60;
    end loop;  
end;
/

create or replace procedure populate_pidreaders as
buildd ref buildingty;
duplies integer;
cursor cs is select ref(b) from building b;
begin
  duplies:=0;
  open cs;
  loop
      fetch cs into buildd;
      select count(*) into duplies from pidreader where buildd=building;
      if duplies=0 then 
        insert into pidreader values (pidreaderty(buildd));
     end if;
  exit when cs%NOTFOUND;
  end loop;
  close cs;
end;
/
create or replace procedure populate_persons as
iteration number;
dept ref deptty;
person_added number;
begin
  iteration:=1;
  person_added:=0;
  loop
        select ref_dept into dept
        from (select ref(d) as ref_dept from dept d order by dbms_random.value)
        where rownum=1;
         person_added:=0;
              loop
              --students
                insert into person values
                      (studentty(dbms_random.string('x',10),
                        dbms_random.string('x',10),dbms_random.string('x',10),
                        to_date(trunc(dbms_random.value(to_char(date '1960-01-01','J'), to_char(date '2000-01-01', 'J'))), 'J'),dept,dbms_random.string('x',6),
                        dbms_random.string('x',10),dbms_random.string('x',3)));   
                        person_added:=person_added+1;
                       exit when person_added=100;
                    end loop;
              --employee
               person_added:=0;
               loop
                  insert into person values
                      (employeety(dbms_random.string('x',10),
                        dbms_random.string('x',10),dbms_random.string('x',10),
                        to_date(trunc(dbms_random.value(to_char(date '1960-01-01','J'),to_char(date '1989-01-01', 'J'))), 'J'),dept,dbms_random.string('x',15),
                        dbms_random.string('l',10), to_date(trunc(dbms_random.value(to_char(sysdate, 'J'), to_char(sysdate+ dbms_random.value(0,1830), 'J'))), 'J')));   
                        person_added:=person_added+1;
                       exit when person_added=100;
                    end loop;
                    iteration := iteration +1;
             exit when iteration = 100;
end loop;

end;
/
create or replace procedure populate_extclosings as
iterations number;
from9_0 ref hourminutesty;
from12_30 ref hourminutesty;
from15_45 ref hourminutesty;
to19_0 ref hourminutesty;
to12_30 ref hourminutesty;
to17_0 ref hourminutesty;
begin

  select ref(r) into from9_0 from hourminutes r where hours=9 and minutes=0;
  select ref(r) into from12_30 from hourminutes r where hours=12 and minutes=30;
  select ref(r) into from15_45 from hourminutes r where hours=15 and minutes=45;
  select ref(r) into to17_0 from hourminutes r where hours=15 and minutes=30;
  select ref(r) into to19_0 from hourminutes r where hours=19 and minutes=0;
  select ref(r) into to12_30 from hourminutes r where hours=12 and minutes=30;
  
  iterations:=0;
  loop
     insert into extclosing values (extclosingty(from9_0,to12_30,to_date(trunc(dbms_random.value(to_char(sysdate, 'J'), to_char(sysdate+ dbms_random.value(0,1830), 'J'))), 'J')));
    iterations:=iterations+1;
    exit when iterations=100;
  end loop;

  iterations:=0;
  loop
  insert into extclosing values (extclosingty(from12_30,to17_0,to_date(trunc(dbms_random.value(to_char(sysdate, 'J'), to_char(sysdate+ dbms_random.value(0,1830), 'J'))), 'J')));
   iterations:=iterations+1;
  exit when iterations=100;
  end loop;
    
  iterations:=0;
  loop
   insert into extclosing values (extclosingty(from15_45,to19_0,to_date(trunc(dbms_random.value(to_char(sysdate, 'J'), to_char(sysdate+ dbms_random.value(0,1830), 'J'))), 'J')));
   iterations:=iterations+1;
  exit when iterations=100;
  end loop;
end;
/
create or replace procedure populate_deptextclosings as
cursor dp is select ref(d) from dept d;
dep_ref ref deptty;
close_ref ref extclosingty;
iterations number;
begin
 open dp;
 loop
 fetch dp into dep_ref;
   iterations:=0;
   loop
      select ref_ext into close_ref
      from (select ref(ext) as ref_ext from extclosing ext order by dbms_random.value)
      where rownum=1;
      insert into deptextclosing values (deptextclosingty(dep_ref,close_ref));
     iterations:=iterations+1;
    exit when iterations=5;
    end loop;
 exit when dp%NOTFOUND;
 end loop;
 close dp;
end;
/

create or replace procedure populate_buildextclosings_sub(bd_ref ref buildingty) as
cursor dpext is select ref(dpext) from deptextclosing dpext;
dept ref deptty;
close_ref ref deptextclosingty;
nomedept varchar2(50);
nomedept2 varchar2(50);
extclosing ref extclosingty;
begin
 open dpext;
 loop
   fetch dpext into close_ref;
     select d.namedept into nomedept from dept d where ref(d)=deref(close_ref).deptextclosing;
     select d.namedept into nomedept2 from dept d where ref(d)=deref(bd_ref).dept;
     select ref(e) into extclosing from extclosing e where ref(e)=deref(close_ref).extclosing;
     if nomedept=nomedept2 then
       insert into buildextclosing values (buildextclosingty(bd_ref,extclosing));
      end if;
    exit when dpext%NOTFOUND;
   end loop;
 close dpext;
end;
/
create or replace procedure populate_buildextclosings as
cursor bd is select ref(b) from building b;
bd_ref ref buildingty;
begin
 open bd;
 loop
 fetch bd into bd_ref;
    populate_buildextclosings_sub(bd_ref);
 exit when bd%NOTFOUND;
 end loop;
 close bd;
end;
/
create or replace procedure populate_pidcards as
cursor ps is select ref(p) from person p;
pers ref personty;
building ref buildingty;
duplies number;
fiscalcodet char(16);
begin
  open ps;
  loop
  duplies:=0;
  fetch ps into pers;
  select count(*) into duplies from pidcard p where deref(pers).fiscalcode=p.person;
  if duplies=0 then
      select fiscalcode into fiscalcodet from person p where deref(pers).fiscalcode=p.fiscalcode;
       insert into pidcard values (pidcardty(fiscalcodet,(cast(multiset(select ref(b) from building b where b.dept=deref(pers).dept) as ref_buildingallowednt))));
  end if;
  duplies:=0;
  exit when ps%NOTFOUND;
  end loop;
  close ps;
end;
/


create or replace procedure populate_entrancesevalidations as
dt date;
extday integer;
rf_pidcard ref pidcardty;
rf_build ref buildingty;
rf_pidreader ref pidreaderty;
timehour timestamp;
var varchar2(50);
duplies number;
pid_readerexists number;
--counters
subiterations number;
iterations number;
num_building number;
evalid integer;
begin
  extday:=0;
  duplies:=0;
  iterations:=0;
  num_building:=0;
  pid_readerexists:=0;
  loop
  subiterations:=0;
   --take a random pidcard
       select ref_pidcard into rf_pidcard
       from (select ref(p) as ref_pidcard from pidcard p order by dbms_random.value)
       where rownum=1;
         -- take a random building
           select count(t.building) into num_building --as ref_build,pidcardID 
            from pidcard p,table(p.buildingallowed) t where p.pidcardID=deref(rf_pidcard).pidcardID;
            if num_building>0 then
             
             loop
                 select ref_build into rf_build
                 from (select t.building as ref_build from pidcard p,table(p.buildingallowed) t where p.pidcardID=deref(rf_pidcard).pidcardID order by dbms_random.value)
                 where rownum=1;
                  --take the pidreader associated to the building
                  select count(*) into pid_readerexists from pidreader p where p.building=rf_build;
                    --dbms_output.put_line(pid_readerexists);
                  if pid_readerexists>0 then 
                   select ref(p) into rf_pidreader from pidreader p where p.building=rf_build;
                   -- dbms_output.put_line(pid_readerexists);
                 --generate a valid date
                   loop
                      dt:=to_date(trunc(dbms_random.value(to_char(sysdate, 'J'), to_char(sysdate+ dbms_random.value(0,1830), 'J'))), 'J');
                      select count(*) into extday from extclosing where datetime=dt;
                    exit when extday=0;
                    end loop;
                    if extday=0 then
                         var:=dt || ' ' || round(dbms_random.value(9,19)) || ':00';
                         timehour:= TO_TIMESTAMP(var,'yyyy/mm/dd hh24:mi');
                        -- dbms_output.put_line(extract(hour from timehour));
                     /*   select count(*) into duplies from evalidation where datehour=timehour and rf_pidcard=pidcard and rf_pidreader=pidreader;
                         if duplies=0 then */
                              insert into evalidation values (evalidationty(timehour,rf_pidcard,rf_pidreader));
                           --  select ref(e) into rf_evalid from evalidation e where datehour=timehour and pidcard=rf_pidcard and pidreader=rf_pidreader;
                              SELECT evalidation_seq.currval INTO evalid FROM DUAL;
                              insert into entrance values (entrancety(evalid,rf_build,timehour));
                      /*  end if;*/
                   end if;
                end if;
                subiterations:=subiterations+1;
                 exit when subiterations=num_building;
                 end loop;
            end if;
         iterations:=iterations+1;
       exit when iterations=10000;
      end loop;
      
        
end;
/
create or replace procedure alter_all_triggers(status varchar2) is
  cursor c_tr is (select 'alter trigger ' || trigger_name as stmnt from user_triggers);
begin
  if status not in ('ENABLE', 'enable', 'DISABLE', 'disable') then
    dbms_output.put_line ('ONLY ''ENABLE DISABLE'' ACCEPTED AS PARAMETERS');
  end if;
  for tr in c_tr loop
    --disable all triggers
    execute immediate tr.stmnt || ' ' || status;
  end loop;
  if status in ('DISABLE', 'disable') then
  --enable some other triggers
    execute immediate 'alter trigger building_incr enable';
    execute immediate 'alter trigger pidreader_incr enable';
    execute immediate 'alter trigger pidcard_incr enable';
    execute immediate 'alter trigger evalidation_incr enable';
  end if;
end;
/

create or replace procedure populate_database as
begin
  alter_all_triggers('DISABLE');
  populate_hourminutes();
  populate_depts();
  populate_persons();
  populate_buildings();
  populate_pidreaders();
  populate_extclosings();
  populate_deptextclosings();
  populate_buildextclosings();
  populate_pidcards();
  populate_entrancesevalidations();
  alter_all_triggers('ENABLE');
end;
/






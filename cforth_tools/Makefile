CF2F=./cf2f.py
F2CF=./f2cf.py
BACK_CF=./OkadBack.cf
WORK_CF=$(DESTDIR)/OkadWork.cf
NEW_CFS=template.cfs

all: work.cf

back.cf: $(BACK_CF)
	cp $(BACK_CF) back.cf

work.cf: $(NEW_CFS) back.cf
	cp back.cf work.cf
	$(F2CF) $(NEW_CFS) work.cf

work.cfs: work.cf
	$(CF2F) work.cf > work.cfs

back.cfs: back.cf
	$(CF2F) back.cf > back.cfs

diff: back.cfs work.cfs
	diff back.cfs work.cfs | tee diff.log

install: work.cf
	cp work.cf $(WORK_CF)
	cp work.cf OkadWork.cf

clean:
	rm -f back.cfs work.cf work.cfs OkadWork.cf

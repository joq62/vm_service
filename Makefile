all:
	rm -rf *.info app_config catalog node_config  logfiles *_service include *~ */*~ */*/*~;
	rm -rf */*.beam;
	rm -rf *.beam erl_crash.dump */erl_crash.dump */*/erl_crash.dump;
#	include
	git clone https://github.com/joq62/include.git;
	cp src/*.app ebin;
	erlc -I include -o ebin src/*.erl;
doc_gen:
	rm -rf  node_config logfiles doc/*;
	erlc ../doc_gen.erl;
	erl -s doc_gen start -sname doc
test:
	rm -rf include *_service include/* *_config erl_crasch.dump;
	git clone https://github.com/joq62/include.git;
#	log_service
	git clone https://github.com/joq62/log_service.git;	
	cp log_service/src/*.app log_service/ebin;
	erlc -I include -o log_service/ebin log_service/src/*.erl;
#	vm_service
	cp src/*.app ebin;
	erlc -I include -o ebin src/*.erl;	
#	test
	erlc -D test -o test_ebin test_src/*.erl;
	erl -config test.config -pa */ebin -pa ebin -pa test_ebin  -s vm_service_tests start -sname vm_test

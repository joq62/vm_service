all:
	rm -rf *.info configs  logfiles *_service include *~ */*~ */*/*~;
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
	rm -rf include configs *_service  erl_crasch.dump;
#	include
	git clone https://github.com/joq62/include.git;
#	config_service
	git clone https://github.com/joq62/config_service.git;	
	cp config_service/src/*.app config_service/ebin;
	erlc -I include -o config_service/ebin config_service/src/*.erl;
#	log_service
	git clone https://github.com/joq62/log_service.git;	
	cp log_service/src/*.app log_service/ebin;
	erlc -I include -o log_service/ebin log_service/src/*.erl;
#	sd_service
	git clone https://github.com/joq62/service_discovery_service.git;	
	cp service_discovery_service/src/*.app service_discovery_service/ebin;
	erlc -I include -o service_discovery_service/ebin service_discovery_service/src/*.erl;
#	vm_service
#	git clone https://github.com/joq62/vm_service.git;	
	cp src/*.app ebin;
	erlc -I include -o ebin src/*.erl;
#	test
	erlc -I include -o test_ebin test_src/*.erl;
	erl -pa ebin -pa */ebin -pa test_ebin -config test.config -s vm_service_tests start -sname vm_test

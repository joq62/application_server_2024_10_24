all:
	#INFO: with_ebin_commit STARTED
	#INFO: Cleaning up to prepare build STARTED	 
	#INFO: Deleting crash reports
	rm -rf erl_cra* rebar3_crashreport;
	rm -rf *~ */*~ */*/*~ */*/*/*~;
	#INFO: Deleting euinit test applications dirs
	rm -rf *_container;
	rm -rf logs;
	rm -rf application_specs;
	rm -rf test_ebin;
	#INFO: Deleting tilde files and beams
	rm -rf *~ */*~ */*/*~;
	rm -rf src/*.beam;
	rm -rf test/*.beam;
	rm -rf *.beam;
	#INFO: Deleting files and dirs created during builds
	rm -rf _build;
	rm -rf rebar.lock
	#INFO: Compile application
	rm -f rebar.config;
	cp src/rebar.config .;
	rm -rf common_include;
	cp -r ~/erlang/simple_system/common_include .
	rebar3 compile
	rm -rf _build*;
	git status
	echo Ok there you go!
	#INFO: no_ebin_commit ENDED SUCCESSFUL
clean:
	#INFO: clean STARTED
	#INFO: with_ebin_commit STARTED
	#INFO: Cleaning up to prepare build STARTED	 
	#INFO: Deleting crash reports
	rm -rf erl_cra* rebar3_crashreport;
	rm -rf *~ */*~ */*/*~ */*/*/*~;
	#INFO: Deleting euinit test applications dirs
	rm -rf *_container;
	rm -rf logs;
	rm -rf test_ebin;
	#INFO: Deleting tilde files and beam
	rm -rf src/*.beam;
	rm -rf test/*.beam;
	rm -rf *.beam;
	#INFO: Deleting files and dirs created during builds
	rm -rf _build;
	rm -rf rebar.lock
	#INFO: Deleting files and dirs created during execution/runtime 
	rm -rf logs;
	rm -rf application_specs;
	#INFO: clean ENDED SUCCESSFUL
eunit: 
	#INFO: eunit STARTED
	#INFO: with_ebin_commit STARTED
	#INFO: Cleaning up to prepare build STARTED	 
	#INFO: Deleting crash reports
	rm -rf erl_cra* rebar3_crashreport;
	rm -rf *~ */*~ */*/*~ */*/*/*~;
	#INFO: Deleting euinit test applications dirs
	rm -rf *_container;
	rm -rf logs;
	rm -rf test_ebin;
	#INFO: Deleting tilde files and beams
	rm -rf src/*.beam;
	rm -rf test/*.beam;
	rm -rf *.beam;
	#INFO: Deleting files and dirs created during builds
	rm -f rebar.config;
	rm -rf _build;
	rm -rf rebar.lock
	#INFO: Deleting files and dirs created during execution/runtime 
	rm -rf logs;
	rm -rf application_specs;
	#INFO: Creating eunit test code using test_ebin dir;
	mkdir test_ebin;
	cp test/test_rebar.config rebar.config;
	rm -rf common_include;
	cp -r ~/erlang/simple_system/common_include .
	#rm test/dependent_apps.erl;
	#cp /home/joq62/erlang/dev_support/dependent_apps.erl test;
	erlc -I include -I common_include -o test_ebin test/*.erl;
	#INFO: Creating Common applications needed for testing
	#INFO: Compile application
	rebar3 compile;
	#INFO: Starts the eunit testing .................
	erl -pa test_ebin\
	 -pa _build/default/lib/log/ebin\
	 -pa _build/default/lib/rd/ebin\
	 -pa _build/default/lib/common/ebin\
	 -pa _build/default/lib/application_server/ebin\
	 -sname test_application_server\
	 -run $(m) start\
	 -setcookie a

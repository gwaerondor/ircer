-record(connection_data, 
	{socket   :: port(),
	 nick     :: string(), 
	 realname :: string(), 
	 server   :: string(), 
	 port     :: integer()
	}).
-record(message, 
	{type     :: ping 
		   | pong
		   | join
		   | notice_auth 
		   | unsupported 
		   | motd 
		   | namelist 
		   | end_of_namelist
		   | topic
		   | mode
		   | nick
		   | nick_in_use
		   | privmsg
		   | ident
		   | quit
		   | part
		   | kick
		   | notice
		   | other,
	 sender   :: string(), 
	 code     :: string(),
	 receiver :: string(), 
	 channel  :: string(), 
	 text     :: string()
	}).
-define(TIMESTAMP, logger:generate_timestamp()).
-define(DATETIMESTAMP, logger:generate_date_time_stamp()).
-define(DATESTAMP, logger:generate_datestamp()).

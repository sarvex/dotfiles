//Modify this file to change what commands output to your statusbar, and recompile using the make command.
static const Block blocks[] = {
	/*Icon*/	/*Command*/	 	/*Update Interval*/	/*Update Signal*/
    {" Kernel: ", "/home/dt/.local/bin/kernel",		    360,		        2},

	{" Uptime: ", "/home/dt/.local/bin/upt",		        60,		            2},

	{" Updates: ", "/home/dt/.local/bin/pacupdate",		360,		        9},
	
	{" Mem: ", "/home/dt/.local/bin/memory",	        6,		            1},

	{" Vol: ", "/home/dt/.local/bin/volume",			2,		            10},

	{" Time: ", "/home/dt/.local/bin/clock",			5,		            0},
};

//sets delimeter between status commands. NULL character ('\0') means no delimeter.
static char delim = '|';

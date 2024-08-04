The program contained within this directory is designed to calculate the demagnetization factor for an ellipsoid containing an arbritrary number of dipoles.
All code included in the directory is compiled and run by executing the bash script run.sh.
If this is not executable then the terminal command 'chmod +x run.sh' will allow it to be run. By default, upon running the program will be user interactive, allowing the user to decide the key values for the calculations.

Upon starting run.sh, the user may see some apparent errors regarding removal of files, this is not an issue, the files are created upon compilation of the code and the removal is intended for repeat runs where changes to code willl occur.

Following this, the user may be prompted as to whether they wish to remove some files, this will be any files with the extension .txt, it is therefore advised the user names any data files with this extension so as to easily update with reruns if desired.

The user should then see a number of messages confirming compilation of the various modules. The current compiler flags are as follows:
	
	-pg		Used for the gprof profiler
	-O3		Optimises the compiled code for speed
	-march=native	Optimises the compiled code for the current hardware detected
	-c		Used for creating module files

These can of course be edited as desired by the user.

Once compilation is complete the user will see the program start. Following this the program will prompt the user to enter the desired value for N (number of dipoles is N**3), the direction of the magnetic dipole moment and as to whether to calculate in serial or parallel.

The main code will then run.

At the bottom of the script are some commented out commands, by uncommenting these and commenting out the lines reading:
		
		"./mainprog.exe"
		"gprof mainprog.exe > profile.txt"

Which then allows the program to be automated for extended running or running on a computer cluster in which the program must be submitted to a queue. If doing this the user must also comment out the individual read lines within the file initialise.f90 and uncomment the read line at the start for all three values.

Upon doing this, the conditions for repeat runs can then be specifed. Currently this is set up so as to run the program 6 times, varying values of N from 1 to 30, in series with the magnetic dipole moment in the x-direction.

When executing like this the program will output all information to the file "out.txt" rather than to the terminal.

More information about specific secitions may be found within the code comments.

/***
 * millipede: FIOptions.h
 * Jess Pumphrey, 2012
 ***/

#ifndef H_MILLIPEDE_FIOPTIONS
#define H_MILLIPEDE_FIOPTIONS

#include <string>
#include <map>
#include <iostream>
#include <fstream>
#include <boost/lexical_cast.hpp> 
using boost::bad_lexical_cast;
using boost::lexical_cast;
using namespace std;

namespace mp {

/**
 * Feature identification parameters
 * 
 * Wraps a string -> integer map and extends it with load/save to files
 * 
 * file format is (KEY\nVALUE).*
 * e.g.
 * KMaxH
 * 120
 * KMinH
 * 90
 * 
 * etc.
 * 
 * bools treated as integers, 1 |-> true, 0 |-> false
 * floats treated as integers to 1 decimal place, x |-> n / 10; n |-> 10x
 * 
 */
class FIOptions
{
public:
	
	bool save(const char * filename) {
		
		ofstream file;
		file.open (filename);
		
		if (file.is_open()) {
		
			for(map<string,int>::iterator it = m_map.begin(); it != m_map.end(); ++it) {
				
				file << it->first << endl;
				file << it->second << endl;
			}
			file.close();
			return true;
		}
		
		cout << "Could not open file to save" << endl;
		return false;
		
	}
	
	bool load(const char * filename) {
		
		ifstream file;
		file.open(filename);
		
		if (file.is_open()) {
			
			string keyLine, valLine;
			int i;
			while ( file.good() )
			{
				getline(file,keyLine);
				getline(file,valLine);
				try
				{
					i = lexical_cast<int>(valLine);	
					m_map[keyLine] = i;
				}
				catch(bad_lexical_cast&)
				{
					cout << "Expected integer value in savefile. File corrupt?" << endl;
					return false;
				}
			}
			
			file.close();
			return true;
			
		}
		
		cout << "Could not open file to load" << endl;
		
		return false;
	}
	
	int& operator[](string s) { return m_map[s]; }
	
private:
	map<string,int> m_map;
	
	
};

}

#endif

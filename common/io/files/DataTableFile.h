/***
 * millipede: DataTableFile.h
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#ifndef H_MILLIPEDE_DATATABLEFILE
#define H_MILLIPEDE_DATATABLEFILE

#include <string>

namespace mp {

//#################### FORWARD DECLARATIONS ####################
class DataTable;

struct DataTableFile
{
	//#################### SAVING METHODS ####################
	static void save_csv(const std::string& filename, const DataTable& table);
	static void save_latex(const std::string& filename, const DataTable& table, bool labelRow = false, const std::string& fontSize = "\\scriptsize");
};

}

#endif

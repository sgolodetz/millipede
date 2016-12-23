/***
 * millipede: DataTableFile.cpp
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#include "DataTableFile.h"

#include <fstream>

#include <boost/algorithm/string/replace.hpp>

#include <common/exceptions/Exception.h>
#include <common/util/DataTable.h>

namespace mp {

//#################### SAVING METHODS ####################
void DataTableFile::save_csv(const std::string& filename, const DataTable& table)
{
	std::ofstream os(filename.c_str(), std::ios_base::binary);
	if(os.fail()) throw Exception("Could not open " + filename + " for writing");

	for(int i=0, rows=table.row_count(); i<rows; ++i)
	{
		for(int j=0, cols=table.column_count(); j<cols; ++j)
		{
			std::string cell = table(i,j);
			boost::replace_all(cell, "\"", "\"\"");		// replace any embedded " with ""

			os << '"' << cell << '"';
			if(j != cols-1) os << ',';
		}
		os << '\n';
	}
}

void DataTableFile::save_latex(const std::string& filename, const DataTable& table, bool labelRow, const std::string& fontSize)
{
	std::ofstream os(filename.c_str(), std::ios_base::binary);
	if(os.fail()) throw Exception("Could not open " + filename + " for writing");

	os << "\\begin{tabular}{";
	for(int j=0, cols=table.column_count(); j<cols; ++j)
	{
		os << 'c';
	}
	os << "}\n";

	for(int i=0, rows=table.row_count(); i<rows; ++i)
	{
		bool labelThisRow = labelRow && i == 0;
		std::string prefix = labelThisRow ? std::string("\\textbf{") : std::string("");
		std::string suffix = labelThisRow ? std::string("}") : std::string("");
		for(int j=0, cols=table.column_count(); j<cols; ++j)
		{
			os << fontSize << ' ' << prefix << table(i,j) << suffix;
			std::string separator = (j != cols-1) ? std::string(" & ") : std::string(" \\\\\n");
			os << separator;
		}

		if(labelThisRow) os << "\\hline\n";
	}

	os << "\\end{tabular}\n";
}

}

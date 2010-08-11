/***
 * millipede: DataTable.h
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#ifndef H_MILLIPEDE_DATATABLE
#define H_MILLIPEDE_DATATABLE

#include <string>
#include <vector>

namespace mp {

class DataTable
{
	//#################### PRIVATE VARIABLES ####################
private:
	std::vector<std::vector<std::string> > m_cells;

	//#################### CONSTRUCTORS ####################
public:
	DataTable(int rows, int cols);

	//#################### PUBLIC OPERATORS ####################
public:
	std::string& operator()(int row, int col);
	const std::string& operator()(int row, int col) const;

	//#################### PUBLIC METHODS ####################
public:
	int column_count() const;
	int row_count() const;
};

}

#endif

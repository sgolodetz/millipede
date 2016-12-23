/***
 * millipede: DataTable.cpp
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#include "util/DataTable.h"

#include <cassert>

namespace mp {

//#################### CONSTRUCTORS ####################
DataTable::DataTable(int rows, int cols)
:	m_cells(rows)
{
	assert(rows > 0);
	assert(cols > 0);

	for(int i=0; i<rows; ++i)
	{
		m_cells[i].resize(cols);
	}
}

//#################### PUBLIC OPERATORS ####################
std::string& DataTable::operator()(int row, int col)
{
	return m_cells[row][col];
}

const std::string& DataTable::operator()(int row, int col) const
{
	return m_cells[row][col];
}

//#################### PUBLIC METHODS ####################
int DataTable::column_count() const
{
	assert(row_count() > 0);
	return static_cast<int>(m_cells[0].size());
}

int DataTable::row_count() const
{
	return static_cast<int>(m_cells.size());
}

}

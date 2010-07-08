/***
 * millipede: CubeTriangleGenerator.h
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#ifndef H_MILLIPEDE_CUBETRIANGLEGENERATOR
#define H_MILLIPEDE_CUBETRIANGLEGENERATOR

#include <common/jobs/SimpleJob.h>

namespace mp {

template <typename Label>
class CubeTriangleGenerator : public SimpleJob
{
	//#################### TYPEDEFS ####################
private:
	typedef MeshBuildingData<Label> MeshBuildingDataT;
	typedef boost::shared_ptr<MeshBuildingDataT> MeshBuildingData_Ptr;

	//#################### PRIVATE VARIABLES ####################
private:
	MeshBuildingData_Ptr m_data;
	int m_x, m_y, m_z;

	//#################### CONSTRUCTORS ####################
public:
	CubeTriangleGenerator(const MeshBuildingData_Ptr& data, int x, int y, int z)
	:	m_data(data), m_x(x), m_y(y), m_z(z)
	{}

	//#################### PUBLIC METHODS ####################
public:
	int length() const
	{
		return 1;
	}

	//#################### PRIVATE METHODS ####################
private:
	void execute_impl()
	{
		// Find the node loops and triangulate them.
		// TODO

		// Ensure that the adjacent node sets for each global node reflect the new edges which have been added during triangulation.
		// TODO
	}
};

}

#endif

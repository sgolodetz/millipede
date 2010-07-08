/***
 * millipede: CubeInternalGenerator.h
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#ifndef H_MILLIPEDE_CUBEINTERNALGENERATOR
#define H_MILLIPEDE_CUBEINTERNALGENERATOR

#include <common/jobs/SimpleJob.h>
#include "MeshBuildingData.h"

namespace mp {

template <typename Label>
class CubeInternalGenerator : public SimpleJob
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
	CubeInternalGenerator(const MeshBuildingData_Ptr& data, int x, int y, int z)
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
		const CubeTable& cubeTable = m_data->cube_table();
		std::set<int> nodeSet = cubeTable.lookup_cube_nodes(m_x, m_y, m_z);
		if(nodeSet.empty()) return;
		std::vector<int> faceCentreNodes = cubeTable.lookup_face_centre_nodes(m_x, m_y, m_z);

		// TODO
	}
};

}

#endif

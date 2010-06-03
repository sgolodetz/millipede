/***
 * millipede: ForestGrid.h
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#ifndef H_MILLIPEDE_FORESTGRID
#define H_MILLIPEDE_FORESTGRID

#include <cassert>
#include <vector>

#include <itkSize.h>

namespace mp {

template <typename Element>
class ForestGrid
{
	//#################### TYPEDEFS ####################
public:
	typedef boost::shared_ptr<Element> Element_Ptr;
	typedef boost::shared_ptr<const Element> Element_CPtr;

	//#################### PRIVATE VARIABLES ####################
private:
	std::vector<Element_Ptr> m_elements;
	itk::Size<3> m_gridSize;
	itk::Size<3> m_subvolumeSize;
	itk::Size<3> m_volumeSize;

	//#################### CONSTRUCTORS ####################
public:
	ForestGrid(const std::vector<Element_Ptr>& elements, const itk::Size<3>& subvolumeSize, const itk::Size<3>& volumeSize)
	{
		initialise(elements, subvolumeSize, volumeSize);
	}

protected:
	ForestGrid()
	{}

	//#################### PUBLIC METHODS ####################
public:
	const Element_Ptr& element(int n)
	{
		assert(0 <= n && n < element_count());
		return m_elements[n];
	}

	Element_CPtr element(int n) const
	{
		assert(0 <= n && n < element_count());
		return m_elements[n];
	}

	int element_count() const
	{
		return static_cast<int>(m_elements.size());
	}

	int element_index_of(int x, int y, int z) const
	{
		int subvolumeX = x / m_subvolumeSize[0];
		int subvolumeY = y / m_subvolumeSize[1];
		int subvolumeZ = z / m_subvolumeSize[2];
		size_t index = (subvolumeZ * m_gridSize[1] + subvolumeY) * m_gridSize[0] + subvolumeX;
		assert(0 <= index && index < m_elements.size());
		return index;
	}

	const Element_Ptr& element_of(int x, int y, int z)
	{
		return m_elements[element_index_of(x,y,z)];
	}

	Element_CPtr element_of(int x, int y, int z) const
	{
		return m_elements[element_index_of(x,y,z)];
	}

	int leaf_index_of(int x, int y, int z) const
	{
		int offsetX = x % m_subvolumeSize[0];
		int offsetY = y % m_subvolumeSize[1];
		int offsetZ = z % m_subvolumeSize[2];
		return (offsetZ * m_subvolumeSize[1] + offsetY) * m_subvolumeSize[0] + offsetX;
	}

	const itk::Size<3>& subvolume_size() const
	{
		return m_subvolumeSize;
	}

	const itk::Size<3>& volume_size() const
	{
		return m_volumeSize;
	}

	//#################### PROTECTED METHODS ####################
protected:
	void initialise(const std::vector<Element_Ptr>& elements, const itk::Size<3>& subvolumeSize, const itk::Size<3>& volumeSize)
	{
		m_elements = elements;
		m_subvolumeSize = subvolumeSize;
		m_volumeSize = volumeSize;

		for(int i=0; i<3; ++i)
		{
			m_gridSize[i] = m_volumeSize[i] / m_subvolumeSize[i];
		}
	}
};

}

#endif

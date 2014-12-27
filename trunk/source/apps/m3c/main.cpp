#include <algorithm>
#include <iostream>
#include <iterator>

#include <itkImageFileReader.h>

#include <common/util/ITKImageUtil.h>
#include <common/visualization/LaplacianSmoother.h>
#include <common/visualization/MeshBuilder.h>
using namespace mp;

template <typename Label>
void output_mesh(std::ostream& os, const Mesh<Label>& mesh)
{
  const std::vector<MeshNode<Label> >& nodes = mesh.nodes();
  for(size_t i = 0, size = nodes.size(); i < size; ++i)
  {
    const MeshNode<Label>& n = nodes[i];

    os << i << ": " << n.position();

    os << " [ ";
    std::copy(n.adjacent_nodes().begin(), n.adjacent_nodes().end(), std::ostream_iterator<int>(os, " "));
    os << "] [ ";

    const std::set<SourcedLabel<Label> >& sourcedLabels = n.sourced_labels();
    for(typename std::set<SourcedLabel<Label> >::const_iterator it = sourcedLabels.begin(), iend = sourcedLabels.end(); it != iend; ++it)
    {
      os << '(' << it->label << ',' << it->source << ") ";
    }

    os << "]\n";
  }

  os << '\n';

  const std::list<MeshTriangle<Label> >& triangles = mesh.triangles();
  for(typename std::list<MeshTriangle<Label> >::const_iterator it = triangles.begin(), iend = triangles.end(); it != iend; ++it)
  {
    for(int j = 0; j < 3; ++j)
    {
      os << it->index(j) << ' ';
    }

    os << "[ ";
    std::copy(it->labels().begin(), it->labels().end(), std::ostream_iterator<Label>(os, " "));
    os << "]\n";
  }
}

int main(int argc, char *argv[])
try
{
  if(argc != 2)
  {
    std::cerr << "Usage: m3c <input filename>\n";
    return EXIT_FAILURE;
  }

  std::string path = argv[1];

	// Load the image.
	typedef itk::Image<int,3> Image;
	typedef itk::ImageFileReader<Image> Reader;
	Reader::Pointer reader = Reader::New();
	reader->SetFileName(path);
	reader->Update();

	// Add a border round it.
	Image::Pointer initialImage = reader->GetOutput();
	itk::Index<3> firstVoxel = {{0,0,0}};
	int borderColour = initialImage->GetPixel(firstVoxel);
	itk::Index<3> borderSize = {{1,1,1}};
	Image::Pointer image = ITKImageUtil::make_bordered_image(initialImage.GetPointer(), borderColour, borderSize);

	// Build the mesh.
	CompositeJob_Ptr job(new CompositeJob);

	MeshBuilder<int> *builder = new MeshBuilder<int>(image->GetLargestPossibleRegion().GetSize(), image);
	job->add_subjob(builder);

#if 0
	LaplacianSmoother<int> *smoother = new LaplacianSmoother<int>(0.5, 6);
	smoother->set_mesh_hook(builder->get_mesh_hook());
	job->add_subjob(smoother);
#endif

	std::map<int,RGBA32> submeshColourMap;
	submeshColourMap.insert(std::make_pair(borderColour, ITKImageUtil::make_rgba32(255, 0, 0, 255)));

	std::map<std::string,int> submeshNameMap;
	itk::ImageRegionConstIterator<Image> it(image, image->GetLargestPossibleRegion());
	for(it.GoToBegin(); !it.IsAtEnd(); ++it)
	{
		int value = it.Get();
		submeshNameMap[boost::lexical_cast<std::string>(value)] = value;
	}
	const size_t MAX_SUBMESH_NAMES = 10;		// an conservative estimate of the number of submesh names that can fit in the visualization window
	if(submeshNameMap.size() > MAX_SUBMESH_NAMES) submeshNameMap.clear();

#if 0
  std::cout << "Building mesh...";
  std::cout.flush();
#endif

  job->execute();

#if 0
  std::cout << "done\n" << std::endl;
#endif

  output_mesh(std::cout, *builder->get_mesh());
  return 0;
}
catch(std::exception& e)
{
  std::cout << e.what() << '\n';
}

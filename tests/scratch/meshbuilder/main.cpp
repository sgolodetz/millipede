/***
 * scratchtest_meshbuilder: main.cpp
 * Copyright Stuart Golodetz, 2010. All rights reserved.
 ***/

#include <millipede/partitionforests/images/AbdominalFeature.h>
#include <millipede/util/ITKImageUtil.h>
#include <millipede/visualization/LaplacianSmoother.h>
#include <millipede/visualization/MeshBuilder.h>
#include <millipede/visualization/MeshDecimator.h>
#include <millipede/visualization/MeshRenderer.h>
using namespace mp;

//#################### TYPEDEFS ####################
typedef int Label;
typedef LaplacianSmoother<Label> LaplacianSmootherT;
typedef Mesh<Label> MeshT;
typedef boost::shared_ptr<MeshT> Mesh_Ptr;
typedef MeshBuilder<Label> MeshBuilderT;
typedef boost::shared_ptr<MeshBuilderT> MeshBuilder_Ptr;
typedef MeshDecimator<Label> MeshDecimatorT;

//#################### FUNCTIONS ####################
void test_simple()
{
#if 1
	Label pixels[] = {
		0,0,0,
		0,0,0,

		1,1,0,
		1,1,0
	};
	MeshBuilderT::LabelImagePointer labelling = ITKImageUtil::make_filled_image<Label>(3, 2, 2, pixels);
#else
	Label pixels[] = {
		0,1,
		0,1,

		2,2,
		3,3
	};
	MeshBuilderT::LabelImagePointer labelling = ITKImageUtil::make_filled_image<Label>(2, 2, 2, pixels);
#endif
	MeshBuilder_Ptr builder(new MeshBuilderT(labelling->GetLargestPossibleRegion().GetSize(), labelling));
	Job::execute_managed(builder);

	Mesh_Ptr mesh = builder->get_mesh();

	std::map<int,RGBA32> submeshColourMap;	// a dummy colour map (there's no point in filling it)
	MeshRenderer renderer(mesh, submeshColourMap);
}

void test_smoothing()
{
	Label pixels[] = {
		0,0,0,
		0,0,0,

		1,1,0,
		1,1,0
	};
	MeshBuilderT::LabelImagePointer labelling = ITKImageUtil::make_filled_image<Label>(3, 2, 2, pixels);

	CompositeJob_Ptr job(new CompositeJob);

	MeshBuilderT *builder = new MeshBuilderT(labelling->GetLargestPossibleRegion().GetSize(), labelling);
	LaplacianSmootherT *smoother = new LaplacianSmootherT(0.5, 6);

	smoother->set_mesh_hook(builder->get_mesh_hook());
	DataHook<Mesh_Ptr> meshHook = builder->get_mesh_hook();

	job->add_subjob(builder);
	job->add_subjob(smoother);
	Job::execute_managed(job);

	Mesh_Ptr mesh = meshHook.get();
}

void test_decimation()
{
#if 0
	Label pixels[] = {
		1,1,1,1,
		1,1,1,1,
		1,1,1,1,

		2,2,2,2,
		2,2,2,2,
		2,2,2,2,
	};
#else
	Label pixels[] = {
		2,2,2,2,
		2,2,2,2,
		2,2,2,2,

		1,1,1,1,
		1,1,1,1,
		1,1,1,1,
	};
#endif
	MeshBuilderT::LabelImagePointer labelling = ITKImageUtil::make_filled_image<Label>(4, 3, 2, pixels);

	CompositeJob_Ptr job(new CompositeJob);

	MeshBuilderT *builder = new MeshBuilderT(labelling->GetLargestPossibleRegion().GetSize(), labelling);
	MeshDecimatorT *decimator = new MeshDecimatorT(85);

	decimator->set_mesh_hook(builder->get_mesh_hook());
	DataHook<Mesh_Ptr> meshHook = builder->get_mesh_hook();

	job->add_subjob(builder);
	job->add_subjob(decimator);
	Job::execute_managed(job);

	Mesh_Ptr mesh = meshHook.get();
}

int main()
{
	test_simple();
	test_smoothing();
	test_decimation();
	return 0;
}

# Kostra grafu (Spanning tree)

Application expects a set of graph edges as its input. Then it tries to find all 
spanning trees of the given graph by adding edge by edge to the tree ensuring no 
cycle is created. The algorithm stops when there are no more edges to add or when 
the tree already contains (vertex_count - 1) edges, meaning that it connects all the 
vertices.

The application finds all acceptable solutions by calling setof function, then 
the solutions are reduced so that no solution can be obtained by permuting another one. 

If no existing spanning trees are found, the output is blank.

## Usage

To build the application, run 'make'. Application can be run using the command:  
    ./flp18-log < {input_file} [> {output_file}]

If no output filename is specified, the program writes output to stdout.

## Testing

The application has been tested on a set of tests, which can be found in subfolder 'Tests',
with the use of a script SpanningTreeTest.py. To run the tests, execute 'make test'. To get
average runtime of testing input files, execute 'make time'. When computing the average run time
of every input, the input is executed 10 times and then the average of times is written to the output.

The testing script and data set is also available at https://github.com/Bihanojko/SpanningTreeTests.

## Test input files
The test input files are located in the Tests/Input subfolder. The tests contain various types of graphs and 
cover also input files with errors.
Test15.in contains only whitespace characters. Test14.in and test13.in contain unconnected graphs.   
Input files test16.in and test18.in contain basic barbell graphs with 6 and 8 vertices. Test17.in contains 
invalid types of line and checks whether the application can successfully skip those. 
The remaining tests cover different types of valid graphs, such as fully connected and path graphs. 

## Run time of input files

| Filename      | Run time [s]  |
| ------------- | ------------- |
| test1.in      | 0.0108        |
| test2.in      | 0.0109        |
| test3.in      | 0.0109        |
| test4.in      | 0.01          |
| test5.in      | 0.0111        |
| test6.in      | 0.0101        |
| test7.in      | 0.01          |
| test8.in      | 0.0113        |
| test9.in      | 0.0198        |
| test10.in     | 0.0105        |
| test11.in     | 0.0126        |
| test12.in     | 0.0137        |
| test13.in     | 0.0109        |
| test14.in     | 0.0105        |
| test15.in     | 0.0107        |
| test16.in     | 0.0121        |
| test17.in     | 0.0107        |
| test18.in     | 0.1101        |

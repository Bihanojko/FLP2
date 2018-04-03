# Kostra grafu (Spanning tree)

Application expects a set of graph edges as its input. Then it tries to find all 
spanning trees of the given graph by adding edge by edge to the tree ensuring no 
cycle is created. The algorithm stops when there are no more edges to add or when 
the tree already contains (vertex_count - 1) edges, meaning that it connects all the 
vertexes.

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

## Run time of input files

| Filename      | Run time [s]  |
| ------------- | ------------- |
| test1.in      | 0.0149        |
| test2.in      | 0.0141        |
| test3.in      | 0.0146        |
| test4.in      | 0.0218        |
| test5.in      | 0.013         |
| test6.in      | 0.0131        |
| test7.in      | 0.0132        |
| test8.in      | 0.0191        |
| test9.in      | 2.0672        |
| test10.in     | 0.0116        |
| test11.in     | 0.0252        |
| test12.in     | 3.5508        |
| test13.in     | 0.8351        |
| test14.in     | 8.4561        |
| test15.in     | 0.0115        |

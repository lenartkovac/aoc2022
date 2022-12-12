// See https://aka.ms/new-console-template for more information

using System.Net;
using System.Runtime.CompilerServices;

public class Program
{
    public static void Main()
    {

        // 1. Parse input
        //var inputFile = "/Users/klynx/Projects/aoc2022/day7/test.txt";
        var inputFile = "/Users/klynx/Projects/aoc2022/day7/input.txt";
        var lines = System.IO.File.ReadAllLines(inputFile).ToList();

        var commands = ParseInputIntoCommands(lines).ToList();
        Console.WriteLine("commands");

        // 2. Build tree from input
        var tree = ConstructTreeFromInput(commands);
        
        // 3. Calculate subtrees
        CalculateSizes(tree);
        
        //PrintTree(tree);
        
        
        // 4. Sum Directories of less than 100k
        var result1 = sumSizes(tree, 100_000);
        Console.WriteLine(result1);
        
        // 5. Find 1 smallest directory that releases enough space
        var deviceSize = 70_000_000;
        var freeSpaceSize = deviceSize - tree.TotalSize;
        var updateSize = 30_000_000;

        var requiredSpace = updateSize - freeSpaceSize;
        Console.WriteLine($"Required to free {requiredSpace} space");

        var result2 = FindMinimumOverThreshold(tree, requiredSpace);
        Console.WriteLine(result2);

    }

    private static int FindMinimumOverThreshold(Node tree, int requiredSpace)
    {
        if (tree.Item.GetType() == typeof(File))
        {
            return int.MaxValue;
        }
        
        var minimum = int.MaxValue;
        if (tree.Children.Any())
        {
            foreach (var child in tree.Children)
            {
                var childMin = FindMinimumOverThreshold(child, requiredSpace);
                if (childMin >= requiredSpace)
                {
                    minimum = Math.Min(minimum, childMin);
                }
            }
        }

        if (tree.TotalSize >= requiredSpace)
        {
            minimum = Math.Min(tree.TotalSize, minimum);
        }

        return minimum;
    }

    private static int sumSizes(Node root, int threshold)
    {
        if (root.Item.GetType() == typeof(File))
        {
            return 0;
        }
        
        int totalSum = 0;
        if (root.Children.Any())
        {
            totalSum += root.Children.Sum(child => sumSizes(child, threshold));
        }
        
        if (root.TotalSize <= threshold)
        {
            totalSum += root.TotalSize;
        }

        return totalSum;
    }

    private static void CalculateSizes(Node tree)
    {
        int sizeOfChildren = 0;
        foreach (var child in tree.Children)
        {
            CalculateSizes(child);
            sizeOfChildren += child.TotalSize;
        }

        tree.TotalSize = tree.Item.Size + sizeOfChildren;
    }

    private static Node ConstructTreeFromInput(IEnumerable<Command> lines, Node? rootNode = null)
    {
        if (!lines.Any())
        {
            return rootNode;
        }
        
        var nextCommand = lines.First();
        switch (nextCommand.Type)
        {
            case "cd" when ((Cd)nextCommand).Target == "..":
                rootNode = rootNode.Parent;
                break;
            case "cd":
                if (rootNode == null)
                {
                    rootNode = new Node
                    {
                        Item = new Directory
                        {
                            Name = ((Cd)nextCommand).Target
                        }
                    };
                }
                else
                {
                    rootNode = rootNode.Children.Single(child => child.Item.Name == ((Cd)nextCommand).Target);
                }
                
                break;
            case "ls":
            {
                var items = ((Ls)nextCommand).CommandResults
                    .Select(CommandResultToFileSystemItem)
                    .Select(item => new Node {Item = item, Parent = rootNode})
                    .ToList();
                rootNode.Children.AddRange(items);
                break;
            }
            default: throw new Exception($"unknown command {nextCommand.Type}");
        }

        ConstructTreeFromInput(lines.Skip(1).ToList(), rootNode);
        return rootNode;
    }

    private static FileSystemItem CommandResultToFileSystemItem(string arg)
    {
        var words = arg.Split(' ');
        if (words[0] == "dir")
        {
            return new Directory { Name = words[1] };
        }

        return new File { Name = words[1], Size = Int32.Parse(words[0]) };
    }

    private static IEnumerable<Command> ParseInputIntoCommands(IEnumerable<string> lines)
    {
        var firstCommand = lines.SkipWhile(line => !line.StartsWith('$')).ToList();
        if (firstCommand.Count == 0)
        {
            return new List<Command>();
        }
        var commandString = firstCommand.First();

        var command = StringToCommand(commandString);

        if (command.Type == "ls")
        {
            ((Ls)command).CommandResults = new List<string>(
               firstCommand 
                .Skip(1)
                .TakeWhile(line => !line.StartsWith('$')));
        }

        return new List<Command> { command }.Concat(ParseInputIntoCommands(firstCommand.Skip(1).ToList()));
    }

    private static Command StringToCommand(string commandInString)
    {
        var commandEls = commandInString.Split(' ');
        return commandEls[1] switch
        {
            "cd" => new Cd { Type = "cd", Target = commandEls[2] },
            "ls" => new Ls {Type = "ls" },
            _ => throw new Exception($"command {commandEls[1]} not recognized")
        };
    }
    
    private static void PrintTree(Node rootNode)
    {
        if (rootNode.Item.GetType() == typeof(File))
        {
            return;
        }
        Console.WriteLine(rootNode);
        foreach (var child in rootNode.Children)
        {
            PrintTree(child);
        }
    }

    private abstract class Command
    {
        public string Type;
    }

    private class Cd : Command
    {
        public string Target { get; set; }

        public override string ToString()
        {
            return $"{Type} {Target}";
        }
    }

    private class Ls : Command
    {
        public IEnumerable<string> CommandResults { get; set; } = new List<string>();

        public override string ToString()
        {
            return $"{Type}";
        }
    }

    private abstract class FileSystemItem
    {
        public string Name { get; set; }
        public int Size { get; init;  }

        public override string ToString()
        {
            return $"({Name}, {Size})";
        }
    };

    private class Directory : FileSystemItem
    {
        public int Size => 0;
    }

    private class File : FileSystemItem {}

    private class Node
    {
        public FileSystemItem Item { get; set; }
        public List<Node> Children { get; set; } = new List<Node>();
        public Node Parent { get; set; }

        public int TotalSize { get; set; } = 0;

        public override string ToString()
        {
            var childStrings = string.Join(',', (Children.Select(node => node.Item?.ToString())));
            return $"({TotalSize}) {Item} -> {childStrings}";
        }
    }
}
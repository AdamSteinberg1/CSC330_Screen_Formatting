#include <stdio.h>
#include <iostream>
#include <fstream>
#include <vector>
using namespace std;

string removeNumbers(string word)
{
  string result = "";
  for(char c : word)
  {
    switch (c)
    {
      case '0':
      case '1':
      case '2':
      case '3':
      case '4':
      case '5':
      case '6':
      case '7':
      case '8':
      case '9':
        //do nothing
        break;
      default:
        result += c;
        break;
    }
  }
  return result;
}

//reads in the file
vector<string> getInput(string filename)
{
  vector<string> words;
  ifstream file;
  file.open(filename);
  if(!file.is_open())
  {
    cout << "Error: Could not open specified file.\n";
    exit(1);
  }

  string word;
  while(file >> word)
  {
    words.push_back(removeNumbers(word));
  }
  return words;
}

int main(int argc, char *argv[])
{
  const int LINE_MAX = 60;
  vector<string> words = getInput(argv[1]);

  string minLine = "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA"; //60 As
  string maxLine = "";
  int minLineNum = -1;
  int maxLineNum = -1;
  string currLine = "";
  int currLineNum = 1;
  for(string word: words)
  {
    if(currLine.size() == 0)
    {
      currLine = word;
      continue;
    }

    if(currLine.size() + word.size() + 1 <= LINE_MAX)
    {
      currLine += " " + word;
    }
    else
    {
      printf("%8d  %s\n", currLineNum, currLine.c_str());
      if(currLine.size() <= minLine.size())
      {
        minLine = currLine;
        minLineNum = currLineNum;
      }
      if(currLine.size() >= maxLine.size())
      {
        maxLine = currLine;
        maxLineNum = currLineNum;
      }
      currLineNum++;
      currLine = word;
    }
  }
  printf("%8d  %s\n", currLineNum, currLine.c_str());
  if(currLine.size() <= minLine.size())
  {
    minLine = currLine;
    minLineNum = currLineNum;
  }
  if(currLine.size() >= maxLine.size())
  {
    maxLine = currLine;
    maxLineNum = currLineNum;
  }

  cout << endl;
  printf("%-7s%-13d%s\n", "LONG", maxLineNum, maxLine.c_str());
  printf("%-7s%-13d%s\n", "SHORT", minLineNum, minLine.c_str());

}

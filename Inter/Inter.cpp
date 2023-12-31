#include <cmath>
#include <cstdlib>
#include <fstream>
#include <iomanip>
#include <iostream>
#include <sstream>
#include <stack>
#include <string>

using namespace std;

struct iden
{
  string nom;
  char cla;
  char tip;
  int di1;
  int di2;
  string apv[1000];
};
struct codigo
{
  string mnemo;
  string dir1;
  string dir2;
};
codigo prgm[10000];
iden tabsim[10000];
int coni = 0;
int conp = 0;
const int ERR = -1;
bool flg = false;
stack<string> pila;

ostream &operator<<(ostream &sale, stack<string> s)
{
  stack<string> paux;
  string sal = "";
  if (s.empty())
  {
    sale << "[]";
    return sale;
  }
  while (s.size() > 1)
  {
    sal = ", " + s.top() + sal;
    paux.push(s.top());
    s.pop();
  }
  sal = "[" + s.top() + sal + "]";
  s.pop();
  while (!paux.empty())
  {
    s.push(paux.top());
    paux.pop();
  }
  sale << sal;
  return sale;
}

void tokeniza(string lin)
{
  if (lin[0] == '@')
    flg = true;
  if (!flg)
  {
    int idx = 0, con = 0;
    string cad = "";
    char c = '\0';
    while ((c = lin[idx++]) != '#')
    {
      if (c != ',')
        cad += c;
      else
      {
        switch (con)
        {
        case 0:
          tabsim[coni].nom = cad;
          break;
        case 1:
          tabsim[coni].cla = cad[0];
          break;
        case 2:
          tabsim[coni].tip = cad[0];
          break;
        case 3:
          tabsim[coni].di1 = atoi(cad.c_str());
          break;
        case 4:
          tabsim[coni].di2 = atoi(cad.c_str());
          break;
        case 5:
          tabsim[coni].apv[tabsim[coni].di1] = cad;
          coni++;
          break;
        }
        cad = "";
        con++;
      }
    }
  }
  else
  {
    int idx = 0, con = 0;
    string cad = "";
    char c = '\0';
    while ((c = lin[idx++]) != '\0')
    {
      if (c == ' ' && con == 0)
      {
        conp = atoi(cad.c_str());
        con++;
        cad = "";
      }
      else if (c == ' ' and con == 1)
      {
        prgm[conp].mnemo = cad;
        con++;
        cad = "";
      }
      else if (c == ',' and con == 2)
      {
        prgm[conp].dir1 = cad;
        con++;
        cad = "";
      }
      else if (c != ' ' and c != 13 and con == 3)
        cad += c;
      else if (con < 3)
        cad += c;
    }
    prgm[conp].dir2 = cad;
  }
}
void leeArch(string entrada)
{
  ifstream fuente(entrada);
  string linea;
  if (fuente.is_open())
  {
    while (getline(fuente, linea))
    {
      tokeniza(linea);
    }
    fuente.close();
  }
}

void impTabla()
{
  cout << "ImpTabla coni=" << coni << endl;
  for (int i = 0; i < coni; i++)
  {
    cout << tabsim[i].nom << ", " << tabsim[i].cla << ", " << tabsim[i].tip
         << ", " << tabsim[i].di1 << ", " << tabsim[i].di2 << ", "
         << tabsim[i].apv[0] << ", #," << endl;
  }
}

int getIden(string id)
{
  for (int i = 0; i < coni; i++)
  {
    if (tabsim[i].nom == id)
      return i;
  }
  return ERR;
}

void exec()
{
  // impTabla();
  int pc = getIden("_P");
  if (pc >= 0)
    pc = tabsim[pc].di1;
  while (pc >= 0 && pc <= conp)
  {
    /*cout << pc << " "
        << prgm[pc].mnemo << " "
        << prgm[pc].dir1 << ", "
        << prgm[pc].dir2 << endl;*/
    // cout << "pila=" << pila << "\n<enter> ";
    /*string s;
    cin >> s;*/
    if (prgm[pc].mnemo == "OPR")
    {
      int opcode = atoi(prgm[pc].dir2.c_str());
      if (opcode == 19)
      {
        int ix = getIden(prgm[pc].dir1);
        if (ix >= 0)
        {
          string entrada;
          cin >> entrada;
          if (tabsim[ix].di1 == 0 and tabsim[ix].di2 == 0)
            tabsim[ix].apv[0] = entrada;
        }
      }
      else if (prgm[pc].dir1 == "0")
      {
        int opcode = atoi(prgm[pc].dir2.c_str());
        double opd, opi, res;
        bool resb, opib, opdb;
        ostringstream cnv;
        // cout << "pila= " << pila << endl;
        if (opcode > 1 and opcode < 15 and opcode != 8)
        {
          opd = strtod(pila.top().c_str(), 0);
          pila.pop();
          opi = strtod(pila.top().c_str(), 0);
          pila.pop();
        }
        else if (opcode == 8)
        {
          opd = strtod(pila.top().c_str(), 0);
          pila.pop();
        }
        else if (opcode == 17)
        {
          if (pila.top() == "V")
            opdb = true;
          else if (pila.top() == "F")
            opdb = false;
          pila.pop();
        }
        else if (opcode > 14 and opcode < 17)
        {
          if (pila.top() == "V")
            opdb = true;
          else if (pila.top() == "F")
            opdb = false;
          pila.pop();
          if (pila.top() == "V")
            opib = true;
          else if (pila.top() == "F")
            opib = false;
          pila.pop();
        }
        switch (opcode)
        {
        case 0:
          exit(0);
          break;
        case 1:
          pc = atoi(pila.top().c_str()) - 1;
          pila.pop();
          break;
        case 2:
          res = opi + opd;
          break;
        case 3:
          res = opi - opd;
          break;
        case 4:
          res = opi * opd;
          break;
        case 5:
          res = opi / opd;
          break;
        case 6:
          res = (int)opi % (int)opd;
          break;
        case 7:
          res = pow(opi, opd);
          break;
        case 8:
          res = -opd;
          break;
        case 9:
          resb = opi < opd;
          break;
        case 10:
          resb = opi > opd;
          break;
        case 11:
          resb = opi <= opd;
          break;
        case 12:
          resb = opi >= opd;
          break;
        case 13:
          resb = opi != opd;
          break;
        case 14:
          resb = opi == opd;
          break;
        case 15:
          resb = opib or opdb;
          break;
        case 16:
          resb = opib and opdb;
          break;
        case 17:
          resb = !opdb;
          break;
        case 18:
          system("clear");
          break;
        case 20:
          cout << pila.top();
          pila.pop();
          break;
        case 21:
          cout << pila.top() << endl;
          pila.pop();
          break;
        }
        if (opcode > 1 and opcode < 8)
        {
          cnv << setprecision(500) << res;
          pila.push(cnv.str());
        }
        else if (opcode > 8 and opcode < 18)
        {
          string rs;
          if (resb)
            rs = "V";
          else
            rs = "F";
          pila.push(rs);
        }
      }
    }
    else if (prgm[pc].mnemo == "LIT")
    {
      string val = prgm[pc].dir1;
      if (val[0] == '"')
        pila.push(val.substr(1, val.size() - 2));
      else
        pila.push(prgm[pc].dir1);
    }
    else if (prgm[pc].mnemo == "JMP")
    {
      string dir2 = prgm[pc].dir2;
      int dir;
      if (dir2[0] == '_')
      {
        int idx = getIden(dir2);
        if (idx >= 0)
          dir = tabsim[idx].di1;
      }
      else
      {
        dir = atoi(dir2.c_str());
      }
      pc = dir - 1;
    }
    else if (prgm[pc].mnemo == "JMC")
    {
      string val = pila.top(), cond = prgm[pc].dir1, dir2 = prgm[pc].dir2;
      int dir;
      pila.pop();
      if (dir2[0] == '_')
      {
        int idx = getIden(dir2);
        if (idx >= 0)
          dir = tabsim[idx].di1;
      }
      else
      {
        dir = atoi(dir2.c_str());
      }
      if (val == cond)
        pc = dir - 1;
    }
    else if (prgm[pc].mnemo == "LOD")
    {
      int idx = getIden(prgm[pc].dir1);
      if (idx >= 0)
      {
        if (tabsim[idx].di1 == 0 && tabsim[idx].di2 == 0 &&
            tabsim[idx].nom[0] != '_')
        {
          pila.push(tabsim[idx].apv[0]);
          // cout << pila << endl;
        }
        else if (tabsim[idx].nom[0] == '_')
        {
          ostringstream cnv;
          cnv << tabsim[idx].di1;
          pila.push(cnv.str());
          // cout << pila << endl;
        }
        else if (tabsim[idx].cla == 'F')
        {
          pila.push(tabsim[idx].apv[0]);
          // cout << pila << endl;
        }
        else if (tabsim[idx].cla == 'V' || tabsim[idx].cla == 'L' ||
                 tabsim[idx].cla == 'R')
        {
          if (tabsim[idx].di1 > 0 && tabsim[idx].di2 > 0)
          {
            int i, j, desp;
            pila.pop();
            j = atoi(pila.top().c_str());
            pila.pop();
            i = atoi(pila.top().c_str());
            pila.pop();
            desp = i * tabsim[idx].di2 + 1;
            pila.push(tabsim[idx].apv[j + desp]);
            // cout << pila << endl;
          }
          else if (tabsim[idx].di1 > 0)
          {
            int i = atoi(pila.top().c_str());
            pila.pop();
            pila.push(tabsim[idx].apv[i + 1]);
            // cout << pila << endl;
          }
        }
      }
    }
    else if (prgm[pc].mnemo == "STO")
    {
      int ix = getIden(prgm[pc].dir2);
      if (ix >= 0)
      {
        if (tabsim[ix].di1 == 0 and tabsim[ix].di2 == 0)
        {
          tabsim[ix].apv[0] = pila.top();
          pila.pop();
          // cout << pila << endl;
        }
        else if (tabsim[ix].cla == 'F')
        {
          tabsim[ix].apv[0] = pila.top();
          pila.pop();
          // cout << pila << endl;
        }
        else if (tabsim[ix].cla == 'V' || tabsim[ix].cla == 'L' ||
                 tabsim[ix].cla == 'R')
        {
          if (tabsim[ix].di1 > 0 && tabsim[ix].di2 > 0)
          {
            int i, j, desp;
            string val = pila.top();
            pila.pop();
            j = atoi(pila.top().c_str());
            pila.pop();
            i = atoi(pila.top().c_str());
            pila.pop();
            desp = i * tabsim[ix].di2 + 1;
            tabsim[ix].apv[j + desp] = val;
          }
          else if (tabsim[ix].di1 > 0)
          {
            string val = pila.top();
            pila.pop();
            int i = atoi(pila.top().c_str());
            pila.pop();
            tabsim[ix].apv[i + 1] = val;
          }
        }
      }
    }
    else if (prgm[pc].mnemo == "CAL")
    {
      int idx = getIden(prgm[pc].dir1);
      if (idx >= 0)
      {
        pc = tabsim[idx].di1 - 1;
      }
    }
    pc++;
  }
}
int main(int argc, char *argv[])
{
  if (argc > 1)
  {
    leeArch(string(argv[1]) + ".eje");
    for (int i = 0; i < coni; i++)
      cout << tabsim[i].nom << "," << tabsim[i].cla << "," << tabsim[i].tip
           << "," << tabsim[i].di1 << "," << tabsim[i].di2 << ","
           << tabsim[i].apv[0] << ",#" << endl;
    cout << "@" << endl;
    for (int i = 1; i <= conp; i++)
      cout << i << " " << prgm[i].mnemo << " " << prgm[i].dir1 << ", "
           << prgm[i].dir2 << endl;
    cout << endl
         << endl
         << "[ Ejecutando " << argv[1] << ".eje"
         << " ]" << endl
         << endl;

    exec();
  }
}
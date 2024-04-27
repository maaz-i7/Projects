/*
this code inputs system of linear equations in string 
format and outputs the type(s) of possible solutions(s).
it converts the augmented matrix and the coefficient
matrix into reduced row echelon form and displays the
results accordingly
*/

#include<bits/stdc++.h>
using namespace std;

//this function just prints the matrix
void print(vector<vector<double>>v,int n,int m,char ch)
{
    int i,j,l=m+1;
    if(ch=='C')
    l=m;
    for(i=0;i<n;i++)
    {
        for(j=0;j<l;j++)
        {
            if(v[i][j]>=0)
            cout<<abs(v[i][j])<<" ";
            else
            cout<<v[i][j]<<" ";
        }
        cout<<"\n";
    }
    cout<<"\n";
}

//this function creates all other elements of the column as 0 by using the created pivot element as 1
void create0(vector<double>&v1,vector<double>v,double f,int size)
{
    int i;
    for(i=0;i<size;i++)
    v1[i]=v1[i]-(f*v[i]);
}

//this function diminishes the entire row to create pivot element as 1
void dim(vector<double>&v,double f,int size)
{
    int i;
    for(i=0;i<size;i++)
    v[i]=(v[i]/f);
}

//this function swaps the two rows
void swap(vector<double>&v1,vector<double>&v2,int size)
{
    int i,temp;
    for(i=0;i<size;i++)
    {
        temp=v1[i];
        v1[i]=v2[i];
        v2[i]=temp;
    }
}

/*
this function converts the matrix into reduced row echelon form
and also does:
1. returns rank of the rref form
2. prints steps of conversion into rref
3. prints the final rref form
*/
int rref(vector<vector<double>>mat,string var[],int n,int m,char ch,char ch1)
{
    int i,j,k,c,p,r,l=m+1;
    c=0;//it stores the row number of the pivot element to be created

    if(ch=='C')//number of columns is reduced by 1 if the matrix is coefficient matrix
    l=m;

    //prints the original matrix
    if((ch=='A'&&ch1=='S'||ch1=='F')||(ch=='C'&&ch1=='S'||ch1=='F'))
    {
        if(ch=='A')
        cout<<"\nThe Augmented Matrix is: \n";
        else if(ch=='C')
        cout<<"\nThe Coefficient Matrix is: \n";
        print(mat,n,m,ch);
    }

    //starts converting into rref form
    for(j=0;j<l;j++)
    {
        p=0;
        for(i=c;i<n;i++)//checks whether all elements of the jth column below the pivot row is 0
        {
            if(mat[i][j]!=0)
            p++;
        }

        if(p==0)//if all elements of the column below the pivot row is 0 then it continues to check the next column
        continue;

        if(c>=n)
        break;//breaks if the pivot row exceeds the number of total rows

        //creation of pivot element as 1 starts
        if(mat[c][j]==0)
        {
            /*
            if the pivot element is zero
            then it checks for the non-zero element
            in the same column below the present row
            and then swaps the two rows
            */
            for(k=c+1;k<n;k++)
            {
                if(mat[k][j]!=0)
                {
                    if((ch=='A'&&ch1=='S')||(ch=='C'&&ch1=='S'))
                    cout<<"R"<<c+1<<"⭤ R"<<k+1<<"\n";
                    
                    swap(mat[c],mat[k],l);//swaps the two rows using the swap()
                    
                    if((ch=='A'&&ch1=='S')||(ch=='C'&&ch1=='S'))
                    print(mat,n,m,ch);
                    break;
                }
            }
        }

        //creates the pivot element as 1 if it is not 1 by dividing the entire row by the pivot element
        if(mat[c][j]!=1)
        {
            if((ch=='A'&&ch1=='S')||(ch=='C'&&ch1=='S'))
            cout<<"R"<<c+1<<"⭢ R"<<c+1<<"/"<<mat[c][j]<<"\n";

            dim(mat[c],mat[c][j],l);//diminishes the pivot row to create 1
            
            if((ch=='A'&&ch1=='S')||(ch=='C'&&ch1=='S'))
            print(mat,n,m,ch);
        }
        //now the pivot element is 1

        //creating all other elements of the column to 0 except the pivot 1
        for(i=0;i<n;i++)
        {
            if(i==c)//skip the pivot element=1
            continue;
            else
            {
                if(mat[i][j]!=0)
                {
                    if((ch=='A'&&ch1=='S')||(ch=='C'&&ch1=='S'))
                    cout<<"R"<<i+1<<"⭢ R"<<i+1<<"-("<<mat[i][j]<<"×R"<<c+1<<")\n";
                    
                    create0(mat[i],mat[c],mat[i][j],l);//creates 0 by the help of created pivot element 1
                    
                    if((ch=='A'&&ch1=='S')||(ch=='C'&&ch1=='S'))
                    print(mat,n,m,ch);
                }
            }
        }
        c++;//increases the pivot position by 1 for every column increased
    }


    //this part calculates the rank of the rref form
    if((ch=='A'||ch=='C')&&ch1!='S'&&ch1!='F')
    {
        r=0;
        k=0;
        
        //checks the first pivot element from end row and stores its column in k
        for(i=n-1;i>=0;i--)
        {
            for(j=0;j<l;j++)
            {
                if(mat[i][j]!=0)
                {
                    k=j+1;
                    break;
                }
            }
            if(k!=0)
            break;
        }

        //calculates rank by counting no. of 1 in columns from columns 0 to column k
        for(j=0;j<k;j++)
        {
            c=0;
            for(i=0;i<n;i++)
            if(mat[i][j]==1)
            c++;
            if(c==1)
            r++;
        }
        return r;//returns rank
    }

    else if(ch=='S')//this part outputs the unique solutions (uses the rref form of the augmented form)
    {
        for(i=0;i<n;i++)
        cout<<var[i]<<" = "<<mat[i][n]<<"\n";
        return 0;
    }

    //prints the final rref form
    if(ch=='A')
    cout<<"Augmented matrix in Reduced Row Echelon Form :\n";
    else if(ch=='C')
    cout<<"Coefficient matrix in Reduced Row Echelon Form :\n";
    print(mat,n,m,ch);
    return 0;
}

double coeff(string eq, string var)//this function extracts coefficient of the variables from the string of equations
{
    double num=0;
    int i,j,f=0;
    string sn="";

    if(var=="constant")//this part returns the constant term of the linear equation
    {
        for(i=eq.length()-1;i>=0;i--)//starts from the end of the equation string and keeps storing char. till it finds '='
        {
            if(eq[i]>=48&&eq[i]<=57||eq[i]=='.'||eq[i]=='-')//makes sure negative and decimal values are included
            sn=sn+eq[i];//keeps storing char. from the string equation from end one by one

            else if(eq[i]=='=')//stops when it finds '='
            {
                reverse(sn.begin(),sn.end());//reverses the string (eg: -1.045 was stored in sn as sn = "540.1-")
                num=stod(sn);//converts the string to double type value by using the inbuilt stod() function 
                break;//breaks as no more char. need to be added
            }
        }
        return num;//returns the constant term of the linear equation
    }

    for(i=0;i<eq.length();i++)//this part extracts the coefficients of the variables from the equation string
    {
        if(eq[i]==var[0])
        {
            if(eq.substr(i,var.length())==var)//checks if it is the right variable whose coefficient needs to be checked
            {
                f++;//f signifies that the variable is present in the equation with non-zero coefficient

                for(j=i-1;j>=0;j--)//starts storing char. from -1 of the index where the variable is present in equation
                {
                    if(eq[j]>=48&&eq[j]<=57||eq[j]=='.')
                    sn=sn+eq[j];//keeps storing char. from the string equation from end one by one

                    else if(eq[j]==' '||eq[j]=='+')//terminates when it finds '=' or '+' or '-'
                    {
                        if(sn=="")//covers the case where coefficient is 1 eg: y x +x +x1
                        return 1;

                        reverse(sn.begin(),sn.end());//reverses the string (eg: -1.045 was stored in sn as sn = "540.1-") 
                        num=stod(sn);//converts the string to double type value by using the inbuilt stod() function
                        break;//breaks as no more char. need to be added
                    }
                    else if(eq[j]=='-')//terminates when it finds '-'
                    {
                        if(sn=="")//covers the case where coefficient is -1 eg: -x -y -x4
                        return -1;
                        sn=sn+'-';//adds '-' to the string if the number is negative
                        reverse(sn.begin(),sn.end());
                        num=stod(sn);//converts the string to double type value by using the inbuilt stod() function
                        break;//breaks as no more char. need to be added
                    }
                }
            }
        }
    }
    if(f==0)
    return 0;//returns 0 if the variable is not present
    else
    return num;
}

int main()
{
    int n,m,i,j,k,c,ct,rab,ra;
    char ch,ch1;
    
    cout<<"\n----------------Simultaneous System of Equations Solver----------------\n";
    
    cout<<"\nInput number of variables : ";
    cin>>m;
    string var[m+1];
    
    cout<<"\nInput "<<m<<" variables : \n";
    for(i=0;i<m;i++)
    cin>>var[i];//input variables from the user so that the code knows what variables' coefficients to check
    var[m]="constant";// stores the last varible as "constant", this informs the coeff() to store the constant term 'd' of the linear equation
    
    cout<<"\nInput number of equations : ";
    cin>>n;
    /*
    case1: variables > equations  m > n matrix rows = m
    case2: variables < equations  m < n matrix rows = n
    case3: variables = equations  m = n matrix rows = m = n
    
    */
    vector<vector<double>>aug(max(m,n));//this stores the augments matrix A|B
    vector<vector<double>>cof(max(m,n));//this stores the coefficient matrix A
    string eq[n];//inputs equations in string format 

    cout<<"\nInput "<<n<<" equations in the format a1x1+a2x2+.....+anxn=d (without spaces) : \n";
    for(i=0;i<n;i++)
    {
        cout<<i+1<<": ";
        cin>>eq[i];//inputs the equations in string format
        eq[i]=' '+eq[i]+' ';//adds space before and after the equation string (helps in storing coefficients in coeff() function)
    }
    
    for(i=0;i<n;i++)
    {
        for(j=0;j<m+1;j++)
        {
            aug[i].push_back(coeff(eq[i],var[j]));//stores the coefficients of the variables by calling coeff() (augmented matrix)
            if(j<m+1)
            cof[i].push_back(coeff(eq[i],var[j]));//stores the coefficients of the variables by calling coeff() (coefficient matrix)
        }
    }

    /*
    we need to handle three cases:
    case1: variables > equations
    case2: variables < equations
    case3: variables = equations
    */

    n=max(m,n);//redefines number of rows of the matrices according to the cases mentioned above
    for(;i<n;i++)// stores 0 in the remaining spaces of the matrices for the case when (variables > equations)
    {
        for(j=0;j<m+1;j++)
        {
            aug[i].push_back(0);
            if(j<m+1)
            cof[i].push_back(0);
        }
    }

    rab=rref(aug,var,n,m,'A','0');//stores the rank of augmented matrix A|B by calling rref()
    ra=rref(cof,var,n,m,'C','0');//stores the rank of coefficient matrix A by calling rref()

    /*
    Theorem used:

    ra <= rab --> consistent(solution(s) possible)
    ----case 1: ra = rab --> unique solutions
    ----case 2: ra < rab --> infintely many solutions
    
    ra > rab --> inconsistent(no solutions possible)

    */
    cout<<"\n-------------------RESULTS:-------------------\n";
    cout<<"\nRank of Coeffecient Matrix = "<<ra<<"\n";
    cout<<"Rank of Augmented Matrix   = "<<rab<<"\n\n";

    if(rab==ra&&ra<=m)//compares the ranks ra and rab and displays results accordingly
    {
        cout<<"Given system of equations is consistent.\n";
        if(ra==m)
        {
            cout<<"There are unique solutions :\n";
            rref(aug,var,m,m,'S','0');
        }
        else if(ra<m)
        cout<<"There are infinitely many solutions.\n";
    }
    else
    cout<<"Given system of equations is inconsistent.\n";

    
    /*
    This part asks the user if they want to print 
    the original / final reduced row echelon form / steps of conversion to reduced row echelon form 
    of the augmented matrix / coefficient matrix
    */
    cout<<"\nDo you want to print the steps of conversion/final reduced row echelon forms? (Input Y/N) : ";
    cin>>ch;
    if(ch=='Y'||ch=='y')
    {
        cout<<"\n";
        cout<<"\tInput 'A' for printing steps of conversion/final reduced row echelon form of Augmented matrix :\n";
        cout<<"\tInput 'C' for printing steps of conversion/final reduced row echelon form of Coefficient matrix :\n";
        cout<<"\tInput 'B' for printing steps of conversion/final reduced row echelon form of both Augmented & Coefficient matrices :\n";
        cout<<"\nInput (A/C/B): ";
        cin>>ch;
        cout<<"\n";
        if(ch=='A'||ch=='a')
        {
            cout<<"\tInput 'S' for printing steps of conversion of Augmented matrix into reduced row echelon form :\n";
            cout<<"\tInput 'F' for printing final reduced row echelon form of Augmented matrix :\n";
            cout<<"\nInput (S/F): ";
            cin>>ch1;
            cout<<"\n";
            if(ch1=='S'||ch1=='s')
            rref(aug,var,n,m,'A','S');
            else if(ch1=='F'||ch1=='f')
            rref(aug,var,n,m,'A','F');
        }
        else if(ch=='C'||ch=='c')
        {
            cout<<"\tInput 'S' for printing steps of conversion of Coefficient matrix into reduced row echelon form :\n";
            cout<<"\tInput 'F' for printing final reduced row echelon form of Coefficient matrix :\n";
            cout<<"\nInput (S/F): ";
            cin>>ch1;
            cout<<"\n";
            if(ch1=='S'||ch1=='s')
            rref(cof,var,n,m,'C','S');
            else if(ch1=='F'||ch1=='f')
            rref(cof,var,n,m,'C','F');
        }
        else if(ch=='B'||ch=='b')
        {
            cout<<"\tInput 'S' for printing steps of conversion of Augmented matrix into reduced row echelon form :\n";
            cout<<"\tInput 'F' for printing final reduced row echelon form of Augmented matrix :\n";
            cout<<"\nInput (S/F): ";
            cin>>ch1;
            cout<<"\n";
            if(ch1=='S'||ch1=='s')
            rref(aug,var,n,m,'A','S');
            else if(ch1=='F'||ch1=='f')
            rref(aug,var,n,m,'A','F');
            cout<<"\tInput 'S' for printing steps of conversion of Coefficient matrix into reduced row echelon form :\n";
            cout<<"\tInput 'F' for printing final reduced row echelon form of Coefficient matrix :\n";
            cout<<"\nInput (S/F): ";
            cin>>ch1;
            cout<<"\n";
            if(ch1=='S'||ch1=='s')
            rref(cof,var,n,m,'C','S');
            else if(ch1=='F'||ch1=='f')
            rref(cof,var,n,m,'C','F');
        }
    } 
    cout<<"\n-------------by-Maaz:)-------------\n\n";
    return 0;
    //end of code.
}
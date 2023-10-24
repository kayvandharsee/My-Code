class University:
    '''
    A class that represents a university with a name, location, motto, and
    date.
    
    Attributes:
        name(str): The name of the university
        location(str): The location of the university
        motto(str): The university's motto
        date(tuple): The university's creation date
    '''
    
    def __init__(self, name = "University", location = "Somewhere",
                 motto = "Something", day = 1, month = 1, year = 2023):
        '''
        The constructor of the University class.
        
        Parameters:
            name(str): The name of the university (default: "University")
            location(str): The location of the university (default:
            "Somewhere")
            motto(str): The motto of the university (default: "Something")
            day(int): The day the university was created (default: 1)
            month(int): The month the university was created in (default: 1)
            year(int): The year the university was created in (default: 2023)
        
        Returns:
            None, creates an object
        
        Raises ValueError:
            if any value in date (day, month, year) is less than or equal to 0
            
        Examples:
            >>> u1 = University("Western University", "London, Ontario",
            "VWe are Western", 7, 3, 1878)
            >>> u1.name
            'Western University'
            
            >>> u2 = University("Queen's University", "Kingston, Ontario",
            "We are Queens", 16, 10, 1841)
            >>> u2.location
            'Kingston, Ontario'

            >>> u3 = University()
            >>> u3.name
            'University'
        '''
        self.name = name
        
        self.location = location
        
        self.motto = motto
        
# This for loop will raise a ValueError if any value in date is less than or
# equal to 0
        
        date = (day, month, year)
        
        for value in date:
            
            if value <= 0:
                
                raise ValueError ("All the values for day, month and year "\
                                  "should be positive")
        
        self.date = date

    def __str__(self):
        '''
        Returns a string representing the University object.

        Returns:
            str: a string representation of the University object
            
        Examples:
            >>> u1 = University("Western University", "London, Ontario",
            "We are Western", 7, 3, 1878)
            >>> str(u1)
            'University of Western University was founded in (7, 3, 1878)
            Its main campus is located in London, Ontario
            Its Motto is: We are Western'
            
            >>> u2 = University("Queen's University", "Kingston, Ontario",
            "We are Queens", 16, 10, 1841)
            >>> str(u2)
            'University of Queen's University was founded in (16, 10, 1841)\nIts main campus is located in Kingston, Ontario\nIts Motto is: Sapientia et Doctrina Stabilitas'

            >>> u3 = University()
            >>> str(u3)
            'University of University was founded in (1, 1, 2023)\nIts main campus is located in Somewhere\nIts Motto is: Something'
        '''
# These lines will create the first, second, and third lines of the string
        line_name_date = ("University of " + self.name + " was founded in " +
                          str(self.date))

        line_location = ("Its main campus is located in " + self.location)
        
        line_motto = ("Its Motto is: " + self.motto)
        
        return (line_name_date + "\n" + line_location + "\n" + line_motto)

class JobPosition:
    '''
    Represents a Job Position with a job_id and description.
    
    Instance Attributes:
        job_id (int): the job ID number of the position
        description (str): the job description based on the job_id
    
    Class Attributes:
        id_description_dict(dict): A dictionary of job positions and the
        range of their job_id's
    '''

    id_description_dict = {"Teaching Assistant": (0,99),
                           "Professor": (100,499),
                           "Faculty Lecturer": (500,999),
                           "Department Chair": (1000,1999),
                           "Dean": (2000,2999),
                           "Vice Principal": (3000,3999),
                           "Principal": 4000}
    
    def id_to_description(self):
        '''
        Assigns the correct description to the instance based on the job_id.

        Returns:
            None, updates the description attribute
        
        Examples:
            >>> job1 = JobPosition(450)
            >>> job1.description
            'Professor'
            
            >>> job2 = JobPosition(2500)
            >>> job2.description
            'Dean'

            >>> job3 = JobPosition(4000)
            >>> job3.description
            'Principal'
        '''

# This loop will assign the correct description depending on the job_id
# The index will make sure to avoid a TypeError
        for index, description in enumerate(self.id_description_dict):
            
            margin = self.id_description_dict[description]

# This if statement will apply to all descriptions besides principal
            if index < (len(self.id_description_dict) - 1):
            
                if (margin[0] <= self.job_id <= margin[1]):
                
                    self.description = description

# This else statement will only apply to the principal description
            else:
                
                if self.job_id == margin:
                
                    self.description = description
                    
    def __init__(self, job_id):
        '''
        The constructor of the JobPosition Class.
        
        Parameters:
            job_id (int): the job ID number of the position
        
        Returns:
            None, creates an object
            
        Examples:
            >>> job1 = JobPosition(450)
            >>> job1.description
            'Professor'
            
            >>> job2 = JobPosition(2500)
            >>> job2.description
            'Dean'

            >>> job3 = JobPosition(4000)
            >>> job3.description
            'Principal'
        '''
        
        self.job_id = job_id
        
        self.id_to_description()
        
    def __str__(self):
        '''
        Returns a the description of the JobPosition object.

        Returns:
            self.description(str): a string representation of the
            JobPosition object
            
        Examples:
            >>> job1 = JobPosition(450)
            >>> str(job1)
            'Professor'
            
            >>> job2 = JobPosition(2500)
            >>> str(job2)
            'Dean'

            >>> job3 = JobPosition(4000)
            >>> str(job3)
            'Principal'
        '''
        
        return self.description
    
    def get_id(self):
        '''
        Retrieves the job_id of the JobPosition instance.

        Returns:
            number: the job_id of the instance
            
        Examples:
            >>> job1 = JobPosition(450)
            >>> job1.get_id()
            450
            
            >>> job2 = JobPosition(2500)
            >>> job2.get_id()
            2500

            >>> job3 = JobPosition(4000)
            >>> job3.get_id()
            4000
        '''
        
        number = self.job_id
        
        return number

    def set_id(self, new_id):
        '''
        Changes the job_id of the JobPosition instance and updates the
        description.
        
        Parameters:
            new_id (int): the new job ID for the job_id attribute
        
        Returns:
            None, updates the job_id and description attributes
            
        Examples:
            >>> job1 = JobPosition(450)
            >>> job1.description
            'Professor'
            >>> job1.set_id(60)
            >>> job1.description
            'Teaching Assistant'
            
            >>> job2 = JobPosition(2500)
            >>> job2.description
            'Dean'
            >>> job2.set_id(3500)
            >>> job2.description
            'Vice Principal'
        '''

        self.job_id = new_id
        
        self.id_to_description()
        
class Employee:
    '''
    Represents an Employee with a first name, last name,
    reference number, job position, and supervisor reference.
    
    Instance Attributes:
        first_name (str): the first name of the employee
        last_name (str): the last name of the employee
        ref (int): the reference number of the employee
        position (JobPosition): the job position of the employee
        supervisor (int): the reference number of the supervisor
    
    Class Attributes:
        nb_employee (int): Keeps track of the number of employees
    '''
    
    nb_employee = 0
    
    def __init__(self, first_name = "Employee", last_name = "Employee",
                 reference = 0, job_id = 4000, sup_ref = -1):
        '''
        The constructor of the Employee Class.
        
        Parameters:
            first_name (str): the first name of the employee
            last_name (str): the last name of the employee
            reference (int): the reference number of the employee
            job_id (int): the job ID number of the position
            sup_ref (int): the reference number of the supervisor
        
        Returns:
            None, creates an object
            
        Examples:
            >>> emp1 = Employee("John", "Doe", 1, 100, -1)
            >>> emp1.first_name
            'John'
            
            >>> emp2 = Employee("Billy", "Smith", 2, 2000, 1)
            >>> emp2.last_name
            'Smith'

            >>> emp3 = Employee("Jimmy", "Green", 3, 3000, 2)
            >>> emp3.ref
            3
        '''
        
        self.nb_employee += 1
        
        self.first_name = first_name
        
        self.last_name = last_name
        
        self.ref = reference
        
        self.position = JobPosition(job_id)
        
        self.supervisor = sup_ref
    
    def __str__(self):
        '''
        Returns a string representation of the Employee object.

        Returns:
            str: a string representation of the Employee object
            
        Examples:
            >>> emp1 = Employee("John", "Doe", 1, 100, -1)
            >>> str(emp1)
            'John Doe - Professor'
            
            >>> emp2 = Employee("Billy", "Smith", 2, 2000, 1)
            >>> str(emp2)
            'Billy Smith - Dean'

            >>> emp3 = Employee("Jim", "Green", 3, 3000, 2)
            >>> str(emp3)
            'Jim Green - Vice Principal'
        '''
        
        return (self.first_name + " " + self.last_name + " - " +
                str(self.position))
    
    def find_supervisor(self, e_list):
        '''
        Finds and returns the Employee object of the supervisor.

        Parameters:
            e_list (list): a list of Employee objects
        
        Returns:
            employee: the supervisor Employee object, or self if no supervisor

        Examples:
            >>> emp1 = Employee("John", "Doe", 1, 100, -1)
            >>> emp2 = Employee("Billy", "Smith", 2, 2000, 1)
            >>> emp_list = [emp1, emp2]
            >>> supervisor = emp2.find_supervisor(emp_list)
            >>> str(supervisor)
            'John Doe - Professor'
        '''

# This if statement will check if the employee has no supervisor
        if self.supervisor == -1:
            
            return self

# This loop will find and return the employees supervisor
        for employee in e_list:
            
            if employee.ref == self.supervisor:
                
                return employee

    def __eq__(self, other):
        '''
        Compares two Employee objects for the same first name, last name,
        and reference number.

        Parameters:
            other (Employee): another Employee object

        Returns:
            bool: True if the two Employee objects have the same first name,
            last name, and reference number; False otherwise

        Examples:
            >>> emp1 = Employee("John", "Doe", 1, 100, -1)
            >>> emp2 = Employee("Billy", "Smith", 2, 2000, 1)
            >>> emp3 = Employee("John", "Doe", 1, 100, -1)
            >>> emp1 == emp2
            False

            >>> emp1 == emp3
            True

            >>> emp2 == Employee("Jane", "Smith", 3, 2000, 1)
            False
        '''
        
        if (self.first_name == other.first_name and
            self.last_name == other.last_name and
            self.ref == other.ref):
            
            return True
        
        return False
    
class OrganizationalChart:
    '''
    Represents an organizational chart for a university with an employee list.
    Attributes:
        university (University): a University object representing the
        university
        employee_list (list): a list of Employee objects representing the
        employees of the university
    '''
    
    def load_chart(self, filepath):
        '''
        Opens, reads, and takes important information from the organizational
        chart from the given file. Then builds the university and
        employee_list attributes

        Parameters:
            filepath (str): the path to the file containing the organizational
            chart data
        
        Returns:
            None, creates an object
        
        Examples:
            >>> load_chart('Mcgill_OrgChart.csv')
            >>> self.employee_list[0]
            <__main__.Employee object at 0x10555c7c0>
            
            >>> load_chart('mcgill_orgchart.txt')
            File does not exist
            
            >>> load_chart(123)
            There was an error with this file
        '''
        
        self.employee_list = []

# This try and except block will catch FileNotFoundErrors, and any other error
        try:
            
            fobj = open(filepath, "r")
            
            content = fobj.readlines()
    
            university_info = content[:4]
            
            all_employee_info = content[7:]

# This loop will get the important info needed from each line
            for index, line in enumerate(university_info):
                
                line = line.strip("\n")
                
                line = line.split(",")
                
                university_info[index] = line
                
# This block of code will format the information properly
            name = university_info[0][1]
            
            location = university_info[1][1] + "," + university_info[1][2]
            
            day = int(university_info[2][1])
            month = int(university_info[2][2])
            year = int(university_info[2][3])
            
            motto = university_info[3][1]

# This line will build the university instance attribute
            self.university = University(name, location, motto, day, month,
                                         year)
 
# This loop will gather the employee info and create an object out of it
            for line in all_employee_info:
                
                line = line.strip("\n")
                
                employee_info = line.split(",")
                
                employee_info[2] = employee_info[2].split()
                
                first_name = employee_info[2][0]
                
                last_name = employee_info[2][1]
                
                reference = int(employee_info[0])
                
                job_id = int(employee_info[1])
                
                sup_reference = int(employee_info[3])
                
# This part will create the employee object and append it to employee_list
                employee = Employee(first_name, last_name, reference,
                                    job_id, sup_reference)
                
                self.employee_list.append(employee)
                
                fobj.close()

# These except blocks will build the instance attributes with the default
# argument values
        except FileNotFoundError:
            
            print("File does not exist")
            
            self.university = University()
            
            self.employee_list.append(Employee())
            
            fobj.close()
            
        except Exception:
            
            print("There was an error with this file")
        
            self.university = University()
            
            self.employee_list.append(Employee())
            
            fobj.close()
            
    def __init__(self, filename):
        '''
        The constructor of the OrganizationalChart class.
        
        Parameters:
            filename (str): the name of the file containing the
            organizational chart data
            
        Returns:
            None, creates an object
            
        Examples:
            >>> load_chart('Mcgill_OrgChart.csv')
            >>> self.employee_list[0]
            <__main__.Employee object at 0x10555c7c0>
            
            >>> load_chart('doesnt_exist.txt')
            File does not exist
            
            >>> load_chart(679)
            There was an error with this file
        '''
        
        self.load_chart(filename)
        
    def __str__(self):
        '''
        Returns a string representing the organizational chart.

        Returns:
            str: a string representation of the organizational chart
        
        Examples:
            >>> chart = OrganizationalChart('Mcgill_OrgChart.csv')
            >>> str(chart)
            '\nH.Deep Saini - Principal\nAngela Campbell - Vice Principal
            \nBruce Lennox - Dean\nGregor Fussmann - Departement Chair
            \nMathieu Blanchette - Departement Chair\nOana Balmau - Professor
            \nJin Guo - Professor\nChristophe Dubach - Professor
            \nGuilia Albertini - Faculty Lecturer\nFaten Mhiri - Faculty
            Lecturer \nJoseph Vybihall - Faculty Lecturer\nJacob Errington -
            Faculty Lecturer \nSaad Yousaf - Teaching Assistant\nMirHamed
            JafarzadehAsl
            - Teaching Assistant\nNeil Rahman - Teaching Assistant'
        '''
        
        string = ''

# This loop will get the name and description of every employee, format it
# correctly, and add it to the string variable which will be returned
        for employee in self.employee_list:
            
            name_description = ("\n" + str(employee))
            
            string = string + name_description
        
        return string
    
    def is_in_list(self, employee):
        '''
        Checks if the given employee is in the employee_list.

        Parameters:
            employee (Employee): an Employee object

        Returns:
            bool: True if the employee is in the employee_list, False otherwise

        Examples:
            >>> chart = OrganizationalChart('Mcgill_OrgChart.csv')
            >>> e1 = Employee('Guilia','Albertini',9,870,5)
            >>> e0 = Employee()
            >>> e3 = Employee('John', 'Doe', 4, 500, 3)
            >>> chart.is_in_list(e1)
            True
            
            >>> chart.is_in_list(e0)
            False
            
            >>> chart.is_in_list(e3)
            False
        '''
        
        return employee in self.employee_list
    
    def find_hierarchical_line(self, employee):
        '''
        Finds the hierarchical line(the supervisors) of the given employee.

        Parameters:
            employee (Employee): an Employee object

        Returns:
            sup_list(list): a list of Employee objects representing the
            hierarchical line
        
        Examples:
            >>> chart = OrganizationalChart('Mcgill_OrgChart.csv')
            >>> e1 = Employee('Guilia','Albertini',9,870,5)
            >>> chart.find_hierarchical_line(e1)
            [<__main__.Employee object at 0x10555c7c0>, <__main__.
            Employee object at 0x10555c7f0>, <__main__.Employee object at
            0x10555c820>, <__main__.Employee object at 0x10555c9a0>, <
            __main__.Employee object at 0x10555c700>]
        '''
        
        sup_list = []

# This section will ensure the employee is inside the list, and return an
# empty list if they are not
        if not self.is_in_list(employee):
            
            return sup_list
        
        sup_list.append(employee)
        
        sup_num = employee.supervisor
        
# This nested loop will make sure that the employees are all listed in order
# It stops when the person with no supervisor is reached
        while sup_num != -1:
        
            for co_worker in self.employee_list:
            
                if co_worker.ref == sup_num:
                
                    sup_list.append(co_worker)
                
                    sup_num = co_worker.supervisor

# The list is flipped to start from the person with no supervisor
        return sup_list[::-1]

    def print_hierarchical_line(self, employee):
        '''
        Prints the hierarchical line of the given employee.

        Parameters:
            employee (Employee): an Employee object
        
        Returns:
            None, prints a string

        Examples:
            >>> chart = OrganizationalChart('Mcgill_OrgChart.csv')
            >>> e1 = Employee('Guilia','Albertini',9,870,5)
            >>> chart.print_hierarchical_line(e1)
            +-> H.Deep Saini - Principal
            | +-> Angela Campbell - Vice Principal
            | | +-> Bruce Lennox - Dean
            | | | +-> Mathieu Blanchette - Department Chair
            | | | | +-> Guilia Albertini - Faculty Lecturer
        '''

# This section calls a function to create the heirarchy list
        sup_list = self.find_hierarchical_line(employee)
        
        employee_chart = ''

# This loop formats the list in the desired way
        for index, worker in enumerate(sup_list):
            
            employee_line = ("| " * index + "+-> " + str(worker) + "\n")
            
            employee_chart += employee_line
        
        print (employee_chart)

chart = OrganizationalChart('Mcgill_OrgChart.csv')
e1 = Employee('Guilia','Albertini',9,870,5)
            >>> chart.print_hierarchical_line(e1)
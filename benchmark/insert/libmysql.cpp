#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <mysql/mysql.h>
#include <pthread.h>
#include <unistd.h>

#define THREAD_NUM  4
#define DBHOST      "localhost"
#define DBUSER      "testMySQLHaskell"
#define DBPASS      ""
#define DBPORT      3306
#define DBNAME      "testMySQLHaskell"
#define DBSOCK      NULL //"/var/lib/mysql/mysql.sock"
#define DBPCNT      0

typedef struct ThreadArgsST
{
    int id;
    pthread_t *thread_id;
} ThreadArgs;

void *func(void *arg)
{
    ThreadArgs *args = (ThreadArgs *)arg;
    MYSQL_RES *result;
    MYSQL_ROW row;
    unsigned int rowCounter = 0;
    MYSQL_FIELD *field;
    unsigned int i;
    unsigned int timeout = 3000;
    const char *pBegin = "BEGIN";
    const char *pCommit = "COMMIT";
    const char *pStatement = "INSERT INTO insert_test values (\
                   0                                      ,\
                   b'1110000010101010'                    ,\
                   -128                                   ,\
                   255                                    ,\
                   -32768                                 ,\
                   65535                                  ,\
                   -8388608                               ,\
                   16777215                               ,\
                   -2147483648                            ,\
                   4294967295                             ,\
                   -9223372036854775808                   ,\
                   18446744073709551615                   ,\
                   1234567890.0123456789                  ,\
                   3.14159                                ,\
                   3.1415926535                           ,\
                   '2016-08-08'                           ,\
                   '2016-08-08 17:25:59'                  ,\
                   '2016-08-08 17:25:59'                  ,\
                   '-199:59:59'                           ,\
                   1999                                   ,\
                   '12345678'                             ,\
                   '韩冬真赞'                             ,\
                   '12345678'                             ,\
                   '12345678'                             ,\
                   '12345678'                             ,\
                   '韩冬真赞'                             ,\
                   '12345678'                             ,\
                   '韩冬真赞'                             ,\
                   'foo'                                  ,\
                   'foo,bar')";
    mysql_thread_init();
    MYSQL *mysql = mysql_init(NULL);

    if (mysql == NULL)
    {
        printf("[%ld][%d]mysql init failed: %s\n", *args->thread_id, args->id, mysql_error(mysql));
        return (void *)0;
    }

    mysql_options(mysql, MYSQL_OPT_CONNECT_TIMEOUT, &timeout);

    if (mysql_real_connect(mysql, DBHOST, DBUSER, DBPASS, DBNAME, DBPORT, DBSOCK, DBPCNT) == NULL)
    {
        printf("[%ld][%d]connect failed: %s\n", *args->thread_id, args->id, mysql_error(mysql));
        mysql_close(mysql);
        mysql_thread_end();
        return (void *)0;
    }
    if (0 != mysql_real_query(mysql, pBegin, strlen(pBegin)))
    {
        printf("[%ld][%d]query failed: %s\n", *args->thread_id, args->id, mysql_error(mysql));
        mysql_close(mysql);
        mysql_thread_end();
        return (void *)0;
    }
    for (i = 0; i < 1000; i++ )
    {
        if (0 != mysql_real_query(mysql, pStatement, strlen(pStatement)))
        {
            printf("[%ld][%d]query failed: %s\n", *args->thread_id, args->id, mysql_error(mysql));
            mysql_close(mysql);
            mysql_thread_end();
            return (void *)0;
        }
    }
    if (0 != mysql_real_query(mysql, pCommit, strlen(pCommit)))
    {
        printf("[%ld][%d]query failed: %s\n", *args->thread_id, args->id, mysql_error(mysql));
        mysql_close(mysql);
        mysql_thread_end();
        return (void *)0;
    }
    mysql_close(mysql);
    mysql_thread_end();
    return (void *)0;
}

int main(int argc, char *argv[])
{
    int thread_num;

    if (argc == 2)
    {
        thread_num = atoi(argv[1]);
    }
    else
    {
        thread_num = THREAD_NUM;
    }

    mysql_library_init(0, NULL, NULL);
    printf("argc: %d and thread_num: %d\n", argc, thread_num);

    do
    {
        pthread_t *pTh = new pthread_t[thread_num];
        ThreadArgs *pArgs = new ThreadArgs[thread_num];
        int i;

        for (i = 0; i < thread_num; i ++)
        {
            pArgs[i].id = i;
            pArgs[i].thread_id = &pTh[i];

            if (0 != pthread_create(&pTh[i], NULL, func, &pArgs[i]))
            {
                printf("pthread_create failed\n");
                continue;
            }
        }

        for (i = 0; i < thread_num; i ++)
        {
            pthread_join(pTh[i], NULL);
        }

        delete[] pTh;
        delete[] pArgs;
    }
    while (0);

    mysql_library_end();
    return 0;
}


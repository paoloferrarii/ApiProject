#include <stdio.h>
#include <stdlib.h>

#define INT_MAXX 2147483647
#define MAX_BUCKET 300


typedef struct air_route {
    int destX;
    int destY;
    struct air_route* next;
}air_route;

typedef struct list_air_route{
    int size;
    air_route* head;
}list_air_route;

typedef struct hex{
    int cost;
    list_air_route routeList;
}hex;

typedef struct mapN{
    int g;
    int visited;
}mapN;

typedef struct bucket{
    int lenght;
    int size;
    int  *nodes;
}bucket;



hex* init(int col, int row){

    if(col<0 || row<0)
        return NULL;
    hex* m = (hex*)malloc(sizeof(hex)*row*col);
    int j=0, i=0;
    for(i=0;i<row;i++){
        for(j=0;j<col;j++){
            m[i*col+j].cost=1;
            m[i*col+j].routeList.head=NULL;
            m[i*col+j].routeList.size=0;
        }
    }
    return m;
}

air_route* add_route( int x, int y){
    air_route* r= (air_route*)malloc(sizeof(air_route));
    r->destX=x;
    r->destY=y;
    r->next=NULL;
    return r;

}



int toggle_air_rout(int x1, int y1, int x2, int y2, hex* m, int col, int row){   //efficetare con puntatri a puntatori

    hex* p;
    air_route* curr,* prec;

      if(x1<0 || y1<0 || x1>=col || y1>=row || x2<0 || y2<0 || x2>=col || y2>=row){   // brutto ma efficenza vediamo
        return 0;
    }
    
    p=&m[y1*col+x1];

    if(p->routeList.size>4){
        return 0;
    }

   if(!p->routeList.size){
        p->routeList.head=add_route(x2,y2);
        p->routeList.size++;
        return 1;
   }else{
        curr=p->routeList.head;
        if(curr->destX==x2 && curr->destY==y2){
            p->routeList.head=curr->next;
            p->routeList.size--;
            free(curr);
        }else{
            prec=curr;
            curr=curr->next;
            while(curr!=NULL){
                if(curr->destX==x2 && curr->destY==y2){
                    prec->next=curr->next;
                    free(curr);
                    p->routeList.size--;
                    return 1;
                }
                prec=curr;
                curr=curr->next;
            }
            prec->next=add_route(x2,y2);
            p->routeList.size++;
        }
   }
   return 1;

}

static inline float f_max(float a, float b){
    return (a>b) ? a : b;
}

static inline int myFloor(float a){
    return (a!=(int)(a) && a<0)?(a-1):a;
}


static inline int calc_cost_plus(int v, int r, int d){
    return  (r!=0)?myFloor(((float)((float)v * f_max(0,((float)(r-d)/(float)r))))):(v);
}

static inline int dist_ass(int x, int y){
    int a=abs(x), b=abs(y), c=abs(x+y);
    return a>b?(a>c?a:c):(b>c?b:c);
}

int change_cost(int x, int y, int v, int r, hex* m, int row, int col){
    int j=0,q=0,p=0,dr=0,dc=0,nr=0,nc=0, *add, zn ;

    if(x<0 || y<0 || x>=col || y>=row || abs(v)>10  || r<1){
        return 0;
    }

    add=(int*)malloc(sizeof(int)*(r));
    for(j=0;j<r;j++){
        add[j]=calc_cost_plus(v,r,j);
    }
        r=r-1;
        for(q=-r;q<=r;q++){
            if(q<=0){
                for(p=-(r+q);p<=r;p++){   // fastidio sistema
                dr=q;
                dc= y&1?(p+(q+(q&1))/2):(p+(q-(q&1))/2);    //controllo se è vero uguale invertito
                nr=y+dr;
                nc=x+dc;
                if(nr>=0 && nr<row && nc>=0 && nc<col){
                    j=dist_ass(p,q);
                    zn=nr*col+nc;
                    m[zn].cost+=add[j];
                    if(m[zn].cost>100)
                        m[zn].cost=100;
                    if(m[zn].cost<0)
                        m[zn].cost=0;
                }
            }
            }else{
                for(p=r-q;p>=-r;p--){
                dr=q;
                dc=y&1?(p+(q+(q&1))/2):(p+(q-(q&1))/2);    //controllo se è vero uguale invertito
                nr=y+dr;
                nc=x+dc;
                zn=nr*col+nc;
                if(nr>=0 && nr<row && nc>=0 && nc<col){
                    j=dist_ass(p,q);
                    zn=nr*col+nc;
                    m[zn].cost+=add[j];
                    if(m[zn].cost>100)
                        m[zn].cost=100;
                    if(m[zn].cost<0)
                        m[zn].cost=0;
                }
            }
            }
                
        }
        free(add);
    return 1;
}



int travel_cost(hex* m, int x1, int y1, int x2, int y2, int col, int row, bucket* bucketList, mapN *map){

    int zc=0,i,j, zn=0, gn, zf, xf, yf, xc,yc, xn, yn,current,nodes_left;
    int dr[6]={0,-1,-1,0,+1,+1};
    int dcd[6]={+1,+1,0,-1,0,+1};
    int dcp[6]={+1,0,-1,-1,-1,0};
    air_route* curr;

    if(x1<0 || y1<0 || y1>=row || x1>=col || x2<0 || y2<0 || y2>=row || x2>=col){   // brutto ma efficenza vediamo
        return -1;
    }
    if(x1==x2 && y1==y2)
        return 0;
    if(m[zc=y1*col+x1].cost==0)
        return -1;


    for(i=0;i<=100;i++)
        bucketList[i].lenght=0;

    for(i=0;i<row;i++){
        for(j=0;j<col;j++){
            map[i*col+j].g=INT_MAXX;
            map[i*col+j].visited=0;
        }
    }



    map[zc].g=0;
    bucketList[0].nodes[0]=zc;
    bucketList[0].lenght=1;
    current=0;
    nodes_left=1;

    while(nodes_left){

        while(bucketList[current].lenght==0){
            current= (current+1)%101;
        }

        zc=bucketList[current].nodes[--bucketList[current].lenght]; 
        nodes_left--;

        map[zc].visited=1;

        if(zc==(y2*col+x2)){
            // ricorda di pulire la lenth dei buck 
            return map[zc].g;
        }
        if(m[zc].cost<=0)
            continue;
        curr=m[zc].routeList.head;
        while(curr!= NULL){
            xf=curr->destX;
            yf=curr->destY;
            zf=yf*col+xf;
            gn=map[zc].g+m[zc].cost;
            if(gn<map[zf].g && !map[zf].visited ){
                map[zf].g=gn;
                i=gn%101;
                if(bucketList[i].lenght<bucketList[i].size){
                    bucketList[i].nodes[bucketList[i].lenght++]=zf;
                }else{
                    bucketList[i].nodes=realloc(bucketList[i].nodes,sizeof(int)*(bucketList[i].size+150));
                    bucketList[i].size+=150;
                }
                
                nodes_left++;
            } 
            curr=curr->next;
        }
        for(i=0;i<6;i++){
            yc=zc/col;
            yn=yc+dr[i];
            xc=zc%col;
            xn=xc+(((yc)&1)==1?dcd[i]:dcp[i]);
            zn=yn*col+xn;
            
            if(xn<0 || xn>=col || yn<0 || yn>=row) // vediamo come va
                continue;
            gn=map[zc].g+m[zc].cost;
            if(!map[zn].visited && gn<map[zn].g ){
                map[zn].g=gn;
                j=gn%101;
                if(bucketList[j].lenght<bucketList[j].size){
                    bucketList[j].nodes[bucketList[j].lenght++]=zn;
                }else{
                    bucketList[j].nodes=realloc(bucketList[j].nodes,sizeof(int)*(bucketList[j].size+150));
                    bucketList[j].size+=150;
                }
                nodes_left++;
            } 
        }
        
    }
    return -1;
}


int strcmp(char* a, char* b){
    int i=0;
    while(a[i]!='\0' && b[i]!='\0'){
        if(a[i]!=b[i])
            return 0;
        i++;
    }
    if(a[i]==b[i])
        return 1;
    else
        return 0;
}



int main(){
    char cmd[50], line[50];
    int x1, x2, y1, y2, v, r, row=0, col=0, i;
    hex* m=NULL;
    bucket *bucketList=malloc(sizeof(bucket)*101);
    for(i=0;i<=100;i++){
        bucketList[i].nodes=malloc(sizeof(int)*MAX_BUCKET);
        bucketList[i].size=MAX_BUCKET;
    }
    mapN* map=NULL;
    
    while(fgets(line,50,stdin)!=NULL){
        sscanf(line,"%s",cmd);
        
        if(cmd[1]=='r'){
            sscanf(line,"%*s %d %d %d %d", &x1, &y1, &x2, &y2);
            printf("%d\n",travel_cost(m,x1,y1,x2,y2,col,row,bucketList,map));

        }else if(cmd[1]=='h'){
            sscanf(line,"%*s %d %d %d %d",&x1, &y1, &v, &r);
            printf("%s", (change_cost(x1,y1,v,r,m,row,col))?"OK\n":"KO\n");
            
        }else if(cmd[1]=='o'){
            sscanf(line,"%*s %d %d %d %d",&x1, &y1, &x2, &y2);
            printf("%s", (toggle_air_rout(x1,y1,x2,y2,m,col,row))?"OK\n":"KO\n");

        }else if(cmd[1]=='n'){
            sscanf(line,"%*s %d %d", &col, &row);
            if(m!= NULL){
                free(m);
                free(map);
            }
                
            m= init(col,row);
            map=malloc(col*row*sizeof(mapN));
            printf("OK\n");
        }
    }

    free(m);
    free (map);
    for(i=0;i<=100;i++)
        free(bucketList[i].nodes);
    free(bucketList);


    return 0;
}



